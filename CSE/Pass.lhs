%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
{-# LANGUAGE PatternGuards #-}

module CSE.Pass ( cseProgram ) where

import CSE.Utilities

import GHCPlugins
import Data.List        ( mapAccumL, find )
\end{code}

                        Simple common sub-expression
                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
        x1 = C a b
        x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
                                 C x1 b -> x2
and apply that to the rest of the program.

When we then see
        y1 = C a b
        y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
        y2 = C y1 b
will get transformed to C x1 b, and then to x2.  

So we carry an extra var->var substitution which we apply *before* looking up in the
reverse mapping.


[Note: SHADOWING]
~~~~~~~~~~~~~~~~~
We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.

However, we do NOT clone type variables.  It's just too hard, because then we need
to run the substitution over types and IdInfo.  No no no.  Instead, we just throw

(In fact, I think the simplifier does guarantee no-shadowing for type variables.)


[Note: case binders 1]
~~~~~~~~~~~~~~~~~~~~~~
Consider

        f = \x -> case x of wild { 
                        (a:as) -> case a of wild1 {
                                    (p,q) -> ...(wild1:as)...

Here, (wild1:as) is morally the same as (a:as) and hence equal to wild.
But that's not quite obvious.  In general we want to keep it as (wild1:as),
but for CSE purpose that's a bad idea.

So we add the binding (wild1 -> a) to the extra var->var mapping.
Notice this is exactly backwards to what the simplifier does, which is
to try to replaces uses of a with uses of wild1

[Note: case binders 2]
~~~~~~~~~~~~~~~~~~~~~~
Consider
        case (h x) of y -> ...(h x)...

We'd like to replace (h x) in the alternative, by y.  But because of
the preceding [Note: case binders 1], we only want to add the mapping
        scrutinee -> case binder
to the reverse CSE mapping if the scrutinee is a non-trivial expression.
(If the scrutinee is a simple variable we want to add the mapping
        case binder -> scrutinee 
to the substitution

[Note: unboxed tuple case binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        case f x of t { (# a,b #) -> 
        case ... of
          True -> f x
          False -> 0 }

We must not replace (f x) by t, because t is an unboxed-tuple binder.
Instead, we shoudl replace (f x) by (# a,b #).  That is, the "reverse mapping" is
        f x --> (# a,b #)
That is why the CSEMap has pairs of expressions.

Note [CSE for INLINE and NOINLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We are careful to do no CSE inside functions that the user has marked as
INLINE or NOINLINE.  In terms of Core, that means 

        a) we do not do CSE inside (Note InlineMe e)

        b) we do not do CSE on the RHS of a binding b=e
           unless b's InlinePragma is AlwaysActive

Here's why (examples from Roman Leshchinskiy).  Consider

        yes :: Int
        {-# NOINLINE yes #-}
        yes = undefined

        no :: Int
        {-# NOINLINE no #-}
        no = undefined

        foo :: Int -> Int -> Int
        {-# NOINLINE foo #-}
        foo m n = n

        {-# RULES "foo/no" foo no = id #-}

        bar :: Int -> Int
        bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we get
yes=no, and the rule does fire.  Worse, whether we get yes=no or
no=yes depends on the order of the definitions.

In general, CSE should probably never touch things with INLINE pragmas
as this could lead to surprising results.  Consider

        {-# INLINE foo #-}
        foo = <rhs>

        {-# NOINLINE bar #-}
        bar = <rhs>        -- Same rhs as foo

If CSE produces
        foo = bar
then foo will never be inlined (when it should be); but if it produces
        bar = foo
bar will be inlined (when it should not be). Even if we remove INLINE foo,
we'd still like foo to be inlined if rhs is small. This won't happen
with foo = bar.

Not CSE-ing inside INLINE also solves an annoying bug in CSE. Consider
a worker/wrapper, in which the worker has turned into a single variable:
        \$wf = h
        f = \x -> ...\$wf...
Now CSE may transform to
        f = \x -> ...h...
But the WorkerInfo for f still says \$wf, which is now dead!  This won't
happen now that we don't look inside INLINEs (which wrappers are).


%************************************************************************
%*                                                                        *
\section{Common subexpression}
%*                                                                        *
%************************************************************************

\begin{code}
cseProgram :: [CoreBind] -> CoreM [CoreBind]
cseProgram binds = return $ cseBinds emptyCSEnv binds

cseBinds :: CSEnv -> [CoreBind] -> [CoreBind]
cseBinds env = snd . mapAccumL cseBind env

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e) = uncurry NonRec `onRightOf` cseOneBind env (b, e)
cseBind env (Rec pairs)  = Rec `onRightOf` mapAccumL cseOneBind env pairs
                        -- The Rec case is suboptimal in that we might try and find a fixed
                        -- point rather than doing the CSE from top to bottom just once

cseOneBind :: CSEnv -> (Id, CoreExpr) -> (CSEnv, (Id, CoreExpr))
cseOneBind env (id, rhs) 
  = case lookupCSEnv env rhs' of
        Just (Var other_id) -> (extendCSEnvIdSubst env' id other_id, (id', Var other_id))
        Just other_expr     -> (env',                                (id', other_expr)  )
        Nothing             -> (addCSEnvItem env' rhs' (Var id'),    (id', rhs')        )
  where
    (env', id') = addBinder env id
    rhs' | isAlwaysActive (idInlineActivation id) = cseExpr env' rhs
         | otherwise                              = rhs
                -- See Note [CSE for INLINE and NOINLINE]

tryForCSE :: CSEnv -> CoreExpr -> CoreExpr
tryForCSE _   (Type t) = Type t
tryForCSE env expr     = (cseExpr env expr) `tryReplace` (lookupCSEnv env)

cseExpr :: CSEnv -> CoreExpr -> CoreExpr
cseExpr _   (Type t)            = Type t
cseExpr _   (Lit lit)           = Lit lit
cseExpr env (Var v)             = Var (lookupCSEnvIdSubst env v)
cseExpr env (App f a)           = App (cseExpr env f) (tryForCSE env a)
cseExpr env (Note n e)          = Note n (cseExpr env e)
cseExpr env (Cast e co)         = Cast (cseExpr env e) co
cseExpr env (Lam b e)           = let (env', b') = addBinder env b
                                  in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)        = let (env', bind') = cseBind env bind
                                  in Let bind' (cseExpr env' e)
cseExpr env (Case scrut bndr ty alts) = let scrut' = tryForCSE env scrut
                                            (env', bndr') = addBinder env bndr
                                        in Case scrut' bndr' ty (cseAlts env' scrut' bndr bndr' alts)


cseAlts :: CSEnv -> CoreExpr -> CoreBndr -> CoreBndr -> [CoreAlt] -> [CoreAlt]

cseAlts env scrut' bndr _bndr' [(DataAlt con, args, rhs)]
  | isUnboxedTupleCon con
        -- Unboxed tuples are special because the case binder isn't
        -- a real value.  See [Note: unboxed tuple case binders]
  = [(DataAlt con, args', tryForCSE new_env rhs)]
  where
    (env', args') = addBinders env args
    new_env = addCSEnvItem env' scrut' tup_value
    tup_value = mkAltExpr (DataAlt con) args' (tyConAppArgs (idType bndr))

cseAlts env scrut' bndr bndr' alts
  = map cseAlt alts
  where
    (con_target, alt_env)
        = case scrut' of
                Var v' -> (v',    extendCSEnvIdSubst env bndr v')    -- See [Note: case binder 1]
                                                                -- map: bndr -> v'
                _      -> (bndr', addCSEnvItem env scrut' (Var  bndr')) -- See [Note: case binder 2]
                                                                        -- map: scrut' -> bndr'
    -- If we are in a data alternative then we have learnt some more expressions we can CSE away.
    -- However, don't try CSE if there are no args; it just increases the number of live vars.  E.g:
    --        case x of { True -> ....True.... }
    -- Don't replace True by x! We deal with this using the usual isExprTrivial check in addCSEnvItem
    cseAlt (con, args, rhs) = (con, args', tryForCSE env'' rhs)
      where
        (env', args') = addBinders alt_env args
        (env'', args'') | (DataAlt _) <- con, let arg_tys = tyConAppArgs (idType bndr)
                        = (addCSEnvItem env' (mkAltExpr con args' arg_tys) (Var con_target), args')
                        | otherwise 
                        = (env', args')
\end{code}

%************************************************************************
%*                                                                        *
\section{The CSE envt}
%*                                                                        *
%************************************************************************

\begin{code}
-- | All the data needed to make the CSE pass tick
data CSEnv = CS CSEMap          -- ^ Expression mapping
                InScopeSet      -- ^ Variables that are in scope, to detect shadowing
                (IdEnv Id)      -- ^ Variable substitutions to apply

-- | This is the reverse mapping: it maps the hash-code of an expression e to 
-- a list of (e, e') pairs. The presence of this mapping means that it's good to 
-- replace e by e'. The occurance of e in the codomain is used to check the hash code.
--
-- INVARIANT: The expr in the range has already been CSE'd
type CSEMap = UniqFM [(CoreExpr, CoreExpr)]

emptyCSEnv :: CSEnv
emptyCSEnv = CS emptyUFM emptyInScopeSet emptyVarEnv

lookupCSEnv :: CSEnv -> CoreExpr -> Maybe CoreExpr
lookupCSEnv (CS cs _ _) expr = do
    pairs <- lookupUFM cs (hashExpr expr)
    lookup_list pairs
  where
    lookup_list :: [(CoreExpr, CoreExpr)] -> Maybe CoreExpr
    lookup_list = fmap snd . find (cheapEqExpr expr . fst)

addCSEnvItem :: CSEnv -> CoreExpr -> CoreExpr -> CSEnv
addCSEnvItem env expr expr' | exprIsBig expr || exprIsCheap expr = env
                            | otherwise                          = extendCSEnv env expr expr'
   -- We don't try to CSE big expressions, because they are unlikely to be the same, and
   -- comparing them is expensive. We also avoid CSEing cheap expressions to keep register
   -- pressure down.
  where
    -- The use of hashExpr here is safe because we check expressions using cheapEqExpr when doing the lookup
    extendCSEnv :: CSEnv -> CoreExpr -> CoreExpr -> CSEnv
    extendCSEnv (CS cs in_scope sub) expr expr' = let cs' = addToUFM_C (++) cs (hashExpr expr) [(expr, expr')]
                                                  in CS cs' in_scope sub

lookupCSEnvIdSubst :: CSEnv -> Id -> Id
lookupCSEnvIdSubst (CS _ _ sub) x = lookupWithDefaultVarEnv sub x x

extendCSEnvIdSubst :: CSEnv -> Id -> Id -> CSEnv
extendCSEnvIdSubst (CS cs in_scope sub) x y = CS cs in_scope (extendVarEnv sub x y)

-- | Register a binder with the CSE environment, which returns the new environment (with suitably modified in-scope
-- set and substitutions) as well as a binder to use in place of the one input from now on
addBinder :: CSEnv -> Id -> (CSEnv, Id)
addBinder (CS cs in_scope sub) v
  | not (v `elemInScopeSet` in_scope)    = (CS cs (extendInScopeSet in_scope v)  sub,                     v )
  | isId v, let v' = uniqAway in_scope v = (CS cs (extendInScopeSet in_scope v') (extendVarEnv sub v v'), v')
        -- Resolve Id shadowing by generating an Id with a new unique to use from this point on
  | otherwise                            = (CS emptyUFM in_scope                 sub,                     v )
        -- This last case is the unusual situation where we have shadowing of a type variable;
        -- we have to discard the CSE mapping. See "IMPORTANT NOTE" at the top

addBinders :: CSEnv -> [Id] -> (CSEnv, [Id])
addBinders env vs = mapAccumL addBinder env vs
\end{code}