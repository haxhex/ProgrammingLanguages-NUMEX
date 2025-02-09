# NUMEX Interpreter and Type System  
This project implements an interpreter and type system for the **NUMEX programming language** using **Racket**. NUMEX is a language with various constructs for arithmetic operations, logical expressions, conditionals, functions, and records. The interpreter evaluates NUMEX programs under different environments, and the type system infers types while checking for errors.  

## **Project Overview**  
- **Language:** NUMEX programs are constructed using a custom syntax and data structures defined in Racket.  
- **Interpreter:** Evaluates NUMEX expressions using custom evaluation rules.  
- **Type System:** Infers types for expressions and ensures type safety.  
- **Extensions:** Includes additional macros and extensions to expand the language's functionality.  

## **Features**  
- Custom NUMEX expressions for arithmetic, boolean logic, conditionals, and functions.  
- Interpreter (`eval-exp`) that evaluates expressions under specified environments.  
- Type inference (`infer-exp`) that statically checks and infers types.  
- NUMEX macros for advanced features like conditionals (`ifmunit`) and recursive functions.  
- Support for user-defined functions and let bindings.  

## **NUMEX Syntax**  
NUMEX supports a range of expressions, including:  
- **Arithmetic:** `(plus e1 e2)`, `(minus e1 e2)`, `(mult e1 e2)`, `(div e1 e2)`  
- **Boolean Operations:** `(andalso e1 e2)`, `(orelse e1 e2)`, `(neg e1)`  
- **Conditionals:** `(cnd e1 e2 e3)`  
- **Equality and Comparisons:** `(iseq e1 e2)`, `(isls e1 e2)`, `(isgt e1 e2)`  
- **Functions:** `(lam s1 s2 e)` and `(apply e1 e2)`  
- **Let Bindings:** `(with s e1 e2)`  
- **Pairs and Lists:** `(apair e1 e2)`, `(1st e1)`, `(2nd e1)`, and list-based operations  
- **Macros:** Custom macros for advanced control flow and list manipulation  

## **How to Run the Project**  
1. Install Racket if not already installed. [Download Racket](https://racket-lang.org/)  
2. Navigate to the project directory.  
3. Open `project.rkt` in the DrRacket IDE or run it via the command line using:  
    ```bash  
    racket project.rkt  
    ```  

## **Key Functions**  
1. **`eval-exp`**: The core interpreter function that evaluates NUMEX expressions under specified environments.  
2. **`infer-exp`**: The type inference function that determines the type of NUMEX expressions and checks for type errors.  
3. **`compute-free-vars`**: Computes free variables in closures for optimizing environments during function application.  
4. **NUMEX Macros**:  
   - **`ifmunit`**: Handles conditionals based on the presence of the `munit` value.  
   - **`with*`**: Allows sequential variable binding in complex expressions.  
   - **`ifneq`**: Handles non-equality checks for decision-making.  

## **Project Deliverables**  
- **NUMEX Interpreter:** Implements the core language constructs.  
- **Type System:** Ensures static type checking and infers types of expressions.  
- **Language Extensions:** Provides macros and additional features for convenience.  
- **Report:** A comprehensive explanation of the language, interpreter, and type system.  

## **Example Usage**  
Here is a sample NUMEX program demonstrating arithmetic and conditionals:  
```racket  
#lang racket  
(require "project.rkt")  

(define expr  
  (with "x" (num 10)  
    (with "y" (num 20)  
      (cnd (isgt (var "x") (var "y"))  
           (num 1)  
           (num 0)))))  

(eval-exp expr '())  
```  
This program checks if `x` is greater than `y` and evaluates to `1` if true, otherwise `0`.  

## **Testing**  
Example programs can be tested by running them through the `eval-exp` function, and their types can be inferred using `infer-exp`. Test cases are provided in the `examples/` directory.  
