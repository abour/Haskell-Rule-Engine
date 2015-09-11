{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module RuleEngine
(
-- Operators
    InsideOp(InsideOp),
    InferiorOp(InferiorOp),

-- Template data
    Unival(Unival),
    CoupleVal(CoupleVal),

-- Template (data + operator)
    Template(Template),

-- Condition
    Condition(Condition),

    EvalTemplate(evalTemplate),

-- Rule
    Rule(Rule),
    addEvaluable,
    evalRule,

-- RuleEngine
    RuleEngine(RuleEngine),
    addRule,
    evalRuleEngine
) where

-- Operators

data InsideOp e = InsideOp e
data InferiorOp e  = InferiorOp e

-- Template data

data Unival x = Unival x
data CoupleVal x = CoupleVal x x

-- Template (data + operator)
data Template dat op = Template dat op

-- Condition

data Condition evaluable = Condition String evaluable

-- Eval type classe

class EvalTemplate val op where
    evalTemplate :: val -> op -> Bool

class EvalCondition e where
    evalCondition :: e -> Bool

instance EvalTemplate (Unival e) (InsideOp i) where
    evalTemplate val op = True

instance Ord(x) => EvalTemplate (CoupleVal x) (InsideOp x) where
    evalTemplate (CoupleVal x1 x2) (InsideOp ins) = ins >= x1 && ins <= x2

instance (EvalTemplate dat op) =>  EvalCondition (Condition (Template dat op)) where
    evalCondition (Condition str (Template v o)) = evalTemplate v o

-- Rule

data Rule evaluable = Rule [evaluable]

addEvaluable :: EvalCondition(e) => Rule e -> e -> Rule e
addEvaluable (Rule lst) val = Rule (val : lst)

evalRule :: EvalCondition(e) => (Rule e) -> Bool
evalRule (Rule evaluables) = foldr (\elem b -> evalCondition(elem) && b) True evaluables

-- Rule Engine

data RuleEngine evaluable = RuleEngine [Rule evaluable]

addRule :: EvalCondition(evaluable) => RuleEngine evaluable -> Rule evaluable -> RuleEngine evaluable
addRule (RuleEngine ruleList) rule = RuleEngine (rule : ruleList)

evalRuleEngine :: EvalCondition(evaluable) => RuleEngine evaluable -> Bool
evalRuleEngine (RuleEngine rules) = foldr (\rule b -> evalRule(rule) && b) True rules