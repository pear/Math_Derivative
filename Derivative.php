<?php

/* vim: set expandtab tabstop=4 shiftwidth=4 softtabstop=4: */

/**
 * Main file of the Math_Derivative PEAR Package
 *
 * PHP version 4 and 5
 * 
 * LICENSE: This source file is subject to version 3.01 of the PHP license
 * that is available through the world-wide-web at the following URI:
 * http://www.php.net/license/3_01.txt.  If you did not receive a copy of
 * the PHP License and are unable to obtain it through the web, please
 * send a note to license@php.net so we can mail you a copy immediately.
 *
 * @category   Math
 * @package    Math_Derivative
 * @author     Etienne Kneuss <colder@php.net>
 * @copyright  1997-2005 The PHP Group
 * @license    http://www.php.net/license/3_01.txt  PHP License 3.01
 * @version    CVS: $Id$
 * @link       http://pear.php.net/package/Math_Derivative
 */
 
// {{{ Math_Derivative

/**
 * This class allows you to calculate the derivative of a mathematical expression.
 *
 * Notice that expressions that are passed to it won't always be valid php syntax :
 * "a^b" means "a raised to the power of b" for Math_Derivative and not the usual 
 * bitwise XOR.
 *
 * That's is important to know, especially if you plan to evaluate the expression  
 * returned using php's own eval() e.g eval($myobject->getDerivative(...));
 *
 * Math_Derivative::getDerivative() will also work on literal expressions.
 *
 * Some functions are already implemented, so you can use them in the input:
 * sin(), cos(), tan(), ln(), log(), e().
 * Use Math_Derivative::registerFunction() to define your own functions.
 *
 * A cache management is already bundled to save intermediary results of the whole 
 * object, it provides a significant speed boost, especially  when used with
 * repetitive expressions. It is also allowed to save the cache and reuse it in 
 * another instance of the class. Its use is optional though.
 * 
 *
 * @category   Math
 * @package    Math_Derivative
 * @author     Etienne Kneuss <colder@php.net>
 * @copyright  1997-2005 The PHP Group
 * @license    http://www.php.net/license/3_0.txt  PHP License 3.0
 * @version    Release: @package_version@
 * @link       http://pear.php.net/package/Math_Derivative
 */

class Math_Derivative {
    // {{{ properties

    /**
     * The variable on which the expression depends
     *
     * @var string
     * @access protected
     * @see Math_Derivative::setVariableName()
     */
    var $_d = 'x';
    
    /**
     * The operators' precedences table. It's not ment to change.
     *
     * @var array
     * @access protected
     */
    var $_operatorsPrecedences = array('+' => 1, '-' => 1, '*' => 2, '/' => 3, '^' => 4);
    
    /**
     * The array that contains already calculated  intermediary expressions
     *
     * @var array
     * @access protected
     */    
    var $_cache = array();
    
    /**
     * Whether to use or not the cache
     *
     * @var boolean
     * @access protected
     */ 
    var $_useCache = true;
    
    /**
     * Contains derivative forms of functions
     * that could appear in expressions
     *
     * @var array
     * @access protected
     */
    var $_registeredFunctions = array('sin'  => 'cos(arg)*d(arg)',
                                      'cos'  => '-sin(arg)*d(arg)',
                                      'tan'  => '1/cos(arg)^2*d(arg)',
                                      'ln'   => 'd(arg)/(arg)',
                                      'log'  => 'd(arg)/(arg)',
                                      'e'    => 'd(arg)*e(arg)',
                                      'sqrt' => 'd((arg)^(1/2))',
                                      'acos' => '-1/((1-(arg)^2)^(1/2))',
                                      'asin' => '1/((1-(arg)^2)^(1/2))',
                                      'atan' => '1/(1+(arg)^2)');
    
    // }}}
    // {{{ setVariableName([(string) $value])
    
    /**
     * Defines the variable on which the expression depends.
     *
     * Only alphabetical characters are allowed. It haven't to be
     * a single char.
     *
     * @param string $value  the string to quote
     *
     * @return boolean  Whether the change was effective
     *
     * @access protected
     * @see Math_Derivative::getDerivative()
     */
    
    function setVariableName($value = 'x')
    {
        if (!preg_match('/^[a-z]+$/i', $value)) {
            return false;
        }
    
        if ($this->_useCache && !empty($this->_cache) && $value !== $this->_d) {
            // restore the cache to avoid cache corruption
            $this->resetCache();
        }

        $this->_d = $value;
        return true;
    }
    
    // }}}
    // {{{ useCache((bool) $flag)
    
    /**
     * Sets if Math_Derivative have to use the caching system.
     *
     *
     * @param boolean $flag 
     *
     * @return boolean true
     *
     * @access public
     */ 
     
    function useCache($flag) 
    {
        $this->_useCache = (bool)$flag;
        
        return true;
    }

    // }}}
    // {{{ resetCache([(array) $cache])
    
    /**
     * Restores/resets the cache.
     *
     *
     * @param array $cache Old cache to restore. 
     *
     * @return boolean Whether the old cache was accepted
     *
     * @access public
     */ 

    function resetCache($cache = array()) 
    {
        if (!is_array($cache)) {
            return false;
        }

        $this->_cache = $cache;
        return true;
    }

    // }}}
    // {{{ getCache()
    
    /**
     * Returns the cache
     *
     *
     * @return array the current cache
     *
     * @access public
     */ 

    function getCache()
    {
        return $this->_cache;
    }
    
    // }}}
    // {{{ getDerivative((string)$expression, (string) $d [, (int) $level = 1])
    
    /**
     * Calculates the derivative of $expression with respect to $d, taken $level times
     *
     *
     * @param string $expression expression of which you want to get the derivative
     * @param string $d          variable on which the expression depends
     * @param int    $level      level of the derivative you want.
     *
     * @return string the derivative
     *
     * @access public
     */ 
     
    function getDerivative ($expression, $d, $level = 1) 
    {
        
        $this->setVariableName($d);

        
        for($i = 0; $i < $level; $i++) {
            $expression = $this->parse($expression);
        }
        return $this->cleanExpression($expression);
    }
    
    // }}}
    // {{{ cleanExpression((string)$expression)
    
    /**
     * Cleans the expression to make the parser's job easier.
     *
     *
     * @param string $expression expression you want to clean
     *
     * @return string cleaned expression
     *
     * @access protected
     */ 

    function cleanExpression($expression) 
    {

        // clean surrounding whitespaces
        $expression = trim($expression);
        
        $deep = 0;
        $deeps = array();

        // clean surounding parenthesis
        
        for ($i = 0; $i < strlen($expression); $i ++) {
            $char = $expression[$i];
            
            if ($char === '(') {
                $deep++;
                continue;
            }
            if ($char === ')') {
                $deep--;
                continue;
            }
            if (!empty($deeps[$deep])) {
                continue;
            }

            $deeps[$deep] = true;

        }
        
        $num_to_delete = min(array_keys($deeps));

        if ($num_to_delete > 0 ) {
            $expression = substr($expression, $num_to_delete, -$num_to_delete);
        }

        return $expression;

    }

    // }}}
    // {{{ parse((string)$expression)
    
    /**
     * Parses the expression and recursively calculates its derivative
     *
     *
     * @param string $expression expressionyou want to parse
     *
     * @return string the derivative
     *
     * @access protected
     */     
    function parse($expression) 
    {
        
        $expression = $this->cleanExpression($expression);

        $initial_expression = $expression;
        
        // if the expression doesn't rely on dx -> 0
        if (!$this->reliesOndx($expression)) {
            return '0';
        }

        // checks if it already exists in the cache
        if ($this->_useCache && isset($this->_cache[$initial_expression])) {
            return $this->_cache[$initial_expression];
        }
        
        // begins parsing
        $depth = 0;

        // array containing the positions of the main operators
        $main_operators = array('precedence' => 0, 'positions' => array());
        
        for ($i=0; $i < strlen($expression); $i++) {
            $char = $expression[$i];

            if ($char === '(') {
                $depth++;
                continue;
            }
            
            if ($char === ')') {
                $depth--;
                continue;
            } 

            if ($depth) {
                continue;
            }
            
            
            if (isset($this->_operatorsPrecedences[$char])) {
                // current char is an operator
                if ($main_operators['precedence'] == $this->_operatorsPrecedences[$char]) {
                    // same type that the main operators : add its position to the list.
                    $main_operators['positions'][] = array($char, $i);
                } else if ($main_operators['precedence'] > $this->_operatorsPrecedences[$char] || !$main_operators['precedence']) {
                    // lower precedence than the list : this operator becomes a main operator
                    $main_operators = array('precedence' => $this->_operatorsPrecedences[$char], 
                                            'positions' => array(array($char, $i)));
                }

            }
        }
        unset($depth);
        
        // splits the expression using operators' positions
        $pos = 0;
        $expression_parts = array();
        foreach ($main_operators['positions'] as $operator) {
            $expression_parts[] = substr($expression, $pos, $operator[1]-$pos);
            $expression_parts[] = $operator[0];
            $pos = $operator[1]+1;
        }
        $expression_parts[] = substr($expression, $pos);
        
        unset($pos);
        
        // dispatchs to the rule corresponding to the main operator
        switch ($main_operators['precedence']) {

            case 1 : // + -
                $expression = $this->ruleAddition($expression_parts);
                break;
                
            case 2 : // *
                $expression = $this->ruleMultiplication($expression_parts);
                break;
                
            case 3: // /
                $expression = $this->ruleDivision($expression_parts);
                break;

            case 4: // ^
                $expression = $this->rulePower($expression_parts);
                break;
            
            default: // term
                $expression = $this->ruleTerm($expression);
                

        }
        
        // put in cache
        if ($this->_useCache) {
            $this->_cache[$initial_expression] = $expression;
        }

        return $expression;
    }
    
    // }}}
    // {{{ reliesOndx((string)$expression)
    
    /**
     * Checks whether the expression relies on d?
     *
     *
     * @param string $expression expression you want to check
     *
     * @return boolean whether $expression relies on d?
     *
     * @access protected
     */  
    
    function reliesOndx($expression) 
    {
        return (bool) preg_match('/\b'.$this->_d.'\b/', $expression);
    }

    // }}}
    // {{{ ruleAddition((array)$parts)
    
    /**
     * Apply the rule of additions
     *   
     *   scheme : (a+b-c)' = a' + b' - c'
     * 
     *
     * @param array $parts parts of the expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */

    function ruleAddition($parts) 
    {
    
        $return_value = '';

        foreach ($parts as $i=>$part) {
            
            if ($i&1) {
                if (strlen($return_value) || $part === '-') {
                    $return_value .= $part;
                }
                continue;
            }

            $derivative = $this->parse($part);
            
            if (empty($derivative)) {
                if (strlen($return_value)) {
                    $return_value = substr($return_value, 0, -1);
                }
                continue;
            }

            $return_value .= $derivative;
        }
        
        return '('.$return_value.')';

    }
    
    // }}}
    // {{{ ruleMultiplication((array)$parts)
    
    /**
     * Apply the rule of multiplications
     *   
     *   scheme : (a*b*c)' = a'*b*c + b'*a*c + c'*a*b
     * 
     *
     * @param array $parts parts of the expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */
     
    function ruleMultiplication($parts)
    {
        
        $terms = array();
        
        foreach ($parts as $i=>$part) {
            if ($i & 1) {
                continue;
            }
            
            $pieces = array();
            $numeric_factor = 1;
            

                
            foreach ($parts as $j=>$otherPart) {
                if ($i === $j || $j & 1 || $otherPart === '1') {
                    continue;
                }
                
                if ($otherPart === '0') {
                    // a factor is = 0 -> whole term ignored
                    continue 2;
                }
                
                if (is_numeric($otherPart)) {
                    $numeric_factor *= $otherPart;
                } else {
                    $pieces[] = $otherPart;
                }
            }


            $deriv = $this->parse($part);
            
            if ($deriv === '0') {
                // deriv = 0 -> whole term ignored
                continue 1;
            }
                
            if (is_numeric($deriv)) {
                $numeric_factor *= $deriv;
            } else {
                $pieces[] = $deriv;
            }

            

            if ($numeric_factor != 1) {
                array_unshift($pieces, $numeric_factor);
            }

            $terms[] = implode('*', $pieces);
        }
        

        return '('.implode('+', $terms).')';
        
    }

    // }}}
    // {{{ ruleDivision((array)$parts)
    
    /**
     * Apply the rule of divisions
     *   
     *   scheme : (a/b/c)' = ((a/b)'/c)'
     *            (a/b)' = (a'*b - b'*a) / b*b
     * 
     *
     * @param array $parts parts of the expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */

    function ruleDivision($parts) 
    {

        if (count($parts) > 3) {
            
            $last_element = array_pop($parts); // save the last term
            array_pop($parts);                 // delete the last /

            $parts = array(implode('', $parts), '/', $last_element);
            return $this->ruleDivision($parts);
        } else {
            return '('.$this->parse($parts[0]).'*'.$parts[2].'-'.$this->parse($parts[2]).'*'.$parts[0].')/('.$parts[2].'*'.$parts[2].')';
        }
        
    }

    // }}}
    // {{{ rulePower((array)$parts)
    
    /**
     * Apply the rule of powers
     *   
     *   scheme : (a^b^c) = ((a^b)^c)
     *            (a^b)'  => 1) b doesn't rely on dx ->  a' * b * a^(b-1)
     *                       2) a and b rely on dx   ->  a^b * ((a'*b)/a + b'*log(a))
     * 
     *
     * @param array $parts parts of the expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */

    function rulePower($parts) 
    {

        if (count($parts) > 3) {

            $last_element = array_pop($parts); // save the last term
            array_pop($parts); // delete the last ^

            $parts = array(implode('', $parts), '^', $last_element);
            return $this->rulePower($parts);
            
        } else {

            if (!$this->reliesOndx($parts[2])) {
                // a' * b * a^(b-1)
                
                $lterm = '*'.$parts[0];
                
                if (is_numeric($parts[2])) {
                    $exp = ($parts[2]-1);
                    if ($exp == 0) {
                        $lterm = '';
                    } elseif ($exp > 1) {
                        $lterm .= '^'.$exp;
                    } else if ($exp < 0) {
                        $lterm .= '^('.$exp.')';
                    }
                    
                    
                } else {
                    $lterm .= '^('.$parts[2].'-1)';
                }
                return $this->parse($parts[0]).'*'.$parts[2].$lterm;
                
            } else {
                // a^b * ((a'*b)/a + b'*log(a))
                return $parts[0].'^'.$parts[2].'* (('.$parts[2].'*'.$this->parse($parts[0]).')/'.$parts[0] .'+ '.$this->parse($parts[2]).'*log('.$parts[0].'))';
            }
        
            
        }
        
    }

    // }}}
    // {{{ registerFunction((string)$name, (string)$derivative)
    
    /**
     * Registers a function to be used in the input
     * e.g. $object->registerFunction('test', 'd(arg)*arg')
     *      arg := the argument of the function
     *      d() := derivative
     *
     * @param string $name       name of the function
     * @param string $derivative derivative form of it 
     *
     * @return bool true
     *
     * @access public
     */
     
    function registerFunction($name, $derivative) 
    {
        $this->_registeredFunctions[$name] = $derivative;
        return true;
    }

    // }}}
    // {{{ getDerivativeCallback((array)$match)
    
    /**
     * Callback used in preg_replace_callback as a recursive way to derivate nested d()'s
     * 
     *
     * @param array $match parts of the expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */
     
    function getDerivativeCallback($match) 
    {
        $expression = preg_replace_callback('/d\s*(\(((?:[^()]+|(?1))+)\))/',  array(&$this, 'getDerivativeCallback'), $match[2]);

        return $this->parse($expression);
        
    }
    
    // }}}
    // {{{ ruleTerm((string)$part)
    
    /**
     * Checks is the term contains a function or if its the variable itself
     * 
     *
     * @param string $part expression
     *
     * @return string expression with the rule applied
     *
     * @access protected
     */
    function ruleTerm($part) 
    {

        if ($part === $this->_d) {
            return 1;
        } else if (preg_match('/^(\w+)\s*(\(((?:[^()]+|(?2))+)\))$/', $part, $match)) {
            // function detected
            
            if (isset($this->_registeredFunctions[$match[1]])) {

                // use the derivative form of a registered function
                $derivative = $this->_registeredFunctions[$match[1]];

                // push $match[3] on 'arg'
                $derivative = preg_replace('/\barg\b/', $match[3], $derivative);
                
                // recursively evaluates d()
                $derivative = preg_replace_callback('/d\s*(\(((?:[^()]+|(?1))+)\))/',  array(&$this, 'getDerivativeCallback'), $derivative);

                return $derivative;
            }
            
        }
        return '('.$part.')\'';
    }

    // }}}  
}

// }}}
?>

