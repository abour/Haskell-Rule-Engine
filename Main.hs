module Main where

import RuleEngine

main = do
    let operator = InsideOp (4::Int)
    let coupleValue = CoupleVal (2::Int) (5::Int)
    let boxing = Template coupleValue operator
    let condition = Condition "Alaintérieure" boxing
    let rule = addEvaluable (Rule []) condition
    let ruleEngine = addRule (RuleEngine []) rule

    print (evalRuleEngine ruleEngine)