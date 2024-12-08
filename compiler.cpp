#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>
#include <stdexcept>
#include <memory>
#include <algorithm>

// Token Types Enumeration
enum TokenType {
    // Arithmetic Operators
    PLUS, MINUS, MULTIPLY, DIVIDE, MODULO,
    
    // Comparison Operators
    EQUAL, NOT_EQUAL, LESS_THAN, GREATER_THAN,
    LESS_EQUAL, GREATER_EQUAL,
    
    // Logical Operators
    LOGICAL_AND, LOGICAL_OR, LOGICAL_NOT,
    
    // Assignment
    ASSIGN,
    
    // Keywords
    IF, ELSE, WHILE, FOR, RETURN,
    INT_TYPE, FLOAT_TYPE, CHAR_TYPE, VOID_TYPE,
    
    // Literals and Identifiers
    IDENTIFIER, 
    INTEGER_LITERAL, 
    FLOAT_LITERAL, 
    STRING_LITERAL,
    
    // Punctuation
    SEMICOLON, COMMA, 
    LEFT_PAREN, RIGHT_PAREN, 
    LEFT_BRACE, RIGHT_BRACE,
    
    // Special
    END_OF_FILE,
    UNKNOWN
};

// Token Structure
struct Token {
    TokenType type;
    std::string value;
    int line;
    int column;
};

// Abstract Syntax Tree Node
class ASTNode {
public:
    virtual ~ASTNode() = default;
    virtual std::string toString() const = 0;
};

// Expression Node
class ExpressionNode : public ASTNode {
public:
    Token token;
    std::string toString() const override {
        return "Expression: " + token.value;
    }
};

// Statement Node
class StatementNode : public ASTNode {
public:
    std::string toString() const override {
        return "Statement";
    }
};

// Lexer Class
class Lexer {
private:
    std::string source;
    size_t current;
    int line;
    int column;

public:
    Lexer(const std::string& src) : source(src), current(0), line(1), column(1) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        
        while (!isAtEnd()) {
            char c = advance();
            
            switch (c) {
                case '+': tokens.push_back(createToken(PLUS, "+")); break;
                case '-': tokens.push_back(createToken(MINUS, "-")); break;
                case '*': tokens.push_back(createToken(MULTIPLY, "*")); break;
                case '/': tokens.push_back(createToken(DIVIDE, "/")); break;
                case '%': tokens.push_back(createToken(MODULO, "%")); break;
                case '=': 
                    tokens.push_back(match('=') ? 
                        createToken(EQUAL, "==") : createToken(ASSIGN, "="));
                    break;
                case '<':
                    tokens.push_back(match('=') ? 
                        createToken(LESS_EQUAL, "<=") : createToken(LESS_THAN, "<"));
                    break;
                case '>':
                    tokens.push_back(match('=') ? 
                        createToken(GREATER_EQUAL, ">=") : createToken(GREATER_THAN, ">"));
                    break;
                case '(':
                    tokens.push_back(createToken(LEFT_PAREN, "("));
                    break;
                case ')':
                    tokens.push_back(createToken(RIGHT_PAREN, ")"));
                    break;
                case '{':
                    tokens.push_back(createToken(LEFT_BRACE, "{"));
                    break;
                case '}':
                    tokens.push_back(createToken(RIGHT_BRACE, "}"));
                    break;
                case ';':
                    tokens.push_back(createToken(SEMICOLON, ";"));
                    break;
                default:
                    if (std::isdigit(c)) {
                        tokens.push_back(recognizeNumber(c));
                    } else if (std::isalpha(c) || c == '_') {
                        tokens.push_back(recognizeIdentifier(c));
                    } else if (std::isspace(c)) {
                        if (c == '\n') {
                            line++;
                            column = 1;
                        }
                    }
            }
        }
        
        tokens.push_back({END_OF_FILE, "", line, column});
        return tokens;
    }

private:
    bool isAtEnd() const {
        return current >= source.length();
    }

    char advance() {
        column++;
        return source[current++];
    }

    bool match(char expected) {
        if (isAtEnd()) return false;
        if (source[current] != expected) return false;
        
        current++;
        column++;
        return true;
    }

    Token createToken(TokenType type, const std::string& value = "") {
        return {type, value, line, column};
    }

    Token recognizeNumber(char first) {
        std::string number;
        number += first;
        
        while (!isAtEnd() && (std::isdigit(source[current]) || source[current] == '.')) {
            number += source[current++];
        }
        
        return {
            number.find('.') != std::string::npos ? FLOAT_LITERAL : INTEGER_LITERAL, 
            number, 
            line, 
            column
        };
    }

    Token recognizeIdentifier(char first) {
        std::string identifier;
        identifier += first;
        
        while (!isAtEnd() && (std::isalnum(source[current]) || source[current] == '_')) {
            identifier += source[current++];
        }
        
        // Keyword recognition
        std::unordered_map<std::string, TokenType> keywords = {
            {"if", IF}, {"else", ELSE}, {"while", WHILE},
            {"for", FOR}, {"return", RETURN}, 
            {"int", INT_TYPE}, {"float", FLOAT_TYPE}, 
            {"char", CHAR_TYPE}, {"void", VOID_TYPE}
        };
        
        auto it = keywords.find(identifier);
        return {
            it != keywords.end() ? it->second : IDENTIFIER, 
            identifier, 
            line, 
            column
        };
    }
};

// Parser Class
class Parser {
private:
    std::vector<Token> tokens;
    size_t current;

public:
    Parser(const std::vector<Token>& tokenList) : tokens(tokenList), current(0) {}

    std::unique_ptr<ASTNode> parse() {
        // Placeholder for parsing logic
        return nullptr;
    }

private:
    bool match(std::initializer_list<TokenType> types) {
        for (auto type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }
        return false;
    }

    bool check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().type == type;
    }

    Token advance() {
        if (!isAtEnd()) current++;
        return previous();
    }

    bool isAtEnd() {
        return peek().type == END_OF_FILE;
    }

    Token peek() {
        return tokens[current];
    }

    Token previous() {
        return tokens[current - 1];
    }
};

// Semantic Analyzer
class SemanticAnalyzer {
public:
    void analyze(ASTNode* root) {
        // Type checking and semantic analysis
    }
};

class IntermediateCodeGenerator {
public:
    struct ThreeAddressCode {
        std::string op;
        std::string arg1;
        std::string arg2;
        std::string result;

        // Method to convert 3-address code to string for display
        std::string toString() const {
            if (arg2.empty()) {
                // Unary operation or assignment
                return result + " = " + arg1;
            } else {
                // Binary operation
                return result + " = " + arg1 + " " + op + " " + arg2;
            }
        }
    };

    std::vector<ThreeAddressCode> generate(ASTNode* root) {
        std::vector<ThreeAddressCode> codes;
        
        // Example 3-address code generation
        codes.push_back({"+", "10", "20", "temp1"});  // temp1 = 10 + 20
        codes.push_back({"*", "temp1", "2", "result"});  // result = temp1 * 2
        
        return codes;
    }

    // Method to print 3-address code
    static void printThreeAddressCode(const std::vector<ThreeAddressCode>& codes) {
        std::cout << "Three-Address Code:\n";
        for (const auto& code : codes) {
            std::cout << code.toString() << std::endl;
        }
    }
};

class AssemblyGenerator {
public:
    std::string generate(const std::vector<IntermediateCodeGenerator::ThreeAddressCode>& intermediateCode) {
        std::stringstream assembly;
        
        assembly << ".intel_syntax noprefix\n";
        assembly << ".global main\n\n";
        
        assembly << "main:\n";
        assembly << "    # Function prologue\n";
        assembly << "    push rbp\n";
        assembly << "    mov rbp, rsp\n\n";
        
        // Generate assembly for intermediate code
        for (const auto& code : intermediateCode) {
            generateInstruction(code, assembly);
        }
        
        // Function epilogue
        assembly << "\n    # Function epilogue\n";
        assembly << "    mov rax, 0   # Return 0\n";
        assembly << "    leave\n";
        assembly << "    ret\n";
        
        return assembly.str();
    }

private:
    void generateInstruction(const IntermediateCodeGenerator::ThreeAddressCode& code, std::stringstream& assembly) {
        if (code.op == "+") {
            assembly << "    # Addition\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    add rax, " << code.arg2 << "\n";
            assembly << "    mov " << code.result << ", rax\n";
        }
        else if (code.op == "*") {
            assembly << "    # Multiplication\n";
            assembly << "    mov rax, " << code.arg1 << "\n";
            assembly << "    imul rax, " << code.arg2 << "\n";
            assembly << "    mov " << code.result << ", rax\n";
        }
    }
};

class Compiler {
public:
    void compile(const std::string& sourceCode) {
        try {
            // Lexical Analysis
            Lexer lexer(sourceCode);
            std::vector<Token> tokens = lexer.tokenize();

            // Parsing
            Parser parser(tokens);
            std::unique_ptr<ASTNode> ast = parser.parse();

            // Semantic Analysis
            SemanticAnalyzer semanticAnalyzer;
            semanticAnalyzer.analyze(ast.get());

            // Intermediate Code Generation
            IntermediateCodeGenerator intermediateGenerator;
            auto threeAddressCode = intermediateGenerator.generate(ast.get());

            // Print 3-Address Code
            IntermediateCodeGenerator::printThreeAddressCode(threeAddressCode);

            // Assembly Code Generation
            AssemblyGenerator assemblyGenerator;
            std::string assemblyCode = assemblyGenerator.generate(threeAddressCode);

            // Output or further processing
            std::cout << "Compilation successful!" << std::endl;
            std::cout << "Generated Assembly Code:\n" << assemblyCode << std::endl;
        } catch (const std::exception& e) {
            std::cerr << "Compilation Error: " << e.what() << std::endl;
        }
    }
};

// Main Function
int main() {
    Compiler compiler;
    
    // Example source code
    std::string sourceCode = R"(
        int main() {
            int x = 10;
            int y = 20;
            int z = x + y;
            return 0;
        }
    )";

    compiler.compile(sourceCode);

    return 0;
}