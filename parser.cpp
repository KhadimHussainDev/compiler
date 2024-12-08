#include <cctype>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

enum TokenType
{
    T_INT,
    T_FLOAT,
    T_ID,
    T_NUM,
    T_IF,
    T_ELSE,
    T_RETURN,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_EOF,
    T_WHILE,
    T_FOR
};
string TokenTypeToString(TokenType type)
{
    switch (type)
    {
    case T_INT:
        return "T_INT";
    case T_FLOAT:
        return "T_FLOAT";
    case T_ID:
        return "T_ID";
    case T_NUM:
        return "T_NUM";
    case T_IF:
        return "T_IF";
    case T_ELSE:
        return "T_ELSE";
    case T_RETURN:
        return "T_RETURN";
    case T_ASSIGN:
        return "T_ASSIGN";
    case T_PLUS:
        return "T_PLUS";
    case T_MINUS:
        return "T_MINUS";
    case T_MUL:
        return "T_MUL";
    case T_DIV:
        return "T_DIV";
    case T_LPAREN:
        return "T_LPAREN";
    case T_RPAREN:
        return "T_RPAREN";
    case T_LBRACE:
        return "T_LBRACE";
    case T_RBRACE:
        return "T_RBRACE";
    case T_SEMICOLON:
        return "T_SEMICOLON";
    case T_GT:
        return "T_GT";
    case T_EOF:
        return "T_EOF";
    case T_FOR:
        return "T_FOR";
    case T_WHILE:
        return "T_WHILE";
    default:
        return "UNKNOWN_TOKEN";
    }
}
void printLineNumber(int line) { cout << "Error on line no: " << line << endl; }
struct Token
{
    TokenType type;
    string value;
    int lineNumber;
};
class Lexer
{
private:
    string src;
    size_t pos;
public:
    Lexer(const string& src)
    {
        this->src = src;
        this->pos = 0;
    }

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        int line = 0;
        bool singleLineComment = false, multiLineComment = false;
        while (pos < src.size())
        {
            char current = src[pos];
            if (current == '\n')
            {
                pos++;
                line++;
                singleLineComment = false;
                continue;
            }
            if (current == '*' && pos + 1 < src.size() && src[pos + 1] == '/')
            {
                multiLineComment = false;
                pos++;
                continue;
            }
            if (singleLineComment || multiLineComment)
            {
                pos++;
                continue;
            }
            if (current == '/' && pos + 1 < src.size())
            {
                if (src[pos + 1] == '/')
                {
                    singleLineComment = true;
                }
                else if (src[pos + 1] == '*')
                {
                    multiLineComment = true;
                }
                pos++;
                continue;
            }
            if (isspace(current))
            {
                pos++;
                continue;
            }
            if (isdigit(current))
            {
                tokens.push_back(Token{ T_NUM, consumeNumber(), line });
                continue;
            }
            if (isalpha(current))
            {
                string word = consumeWord();
                if (word == "int")
                    tokens.push_back(Token{ T_INT, word, line });
                else if (word == "float")
                    tokens.push_back(Token{ T_FLOAT, word, line });
                else if (word == "if")
                    tokens.push_back(Token{ T_IF, word, line });
                else if (word == "else")
                    tokens.push_back(Token{ T_ELSE, word, line });
                else if (word == "return")
                    tokens.push_back(Token{ T_RETURN, word, line });
                else if (word == "for")
                {
                    tokens.push_back(Token{ T_FOR, word, line });
                }
                else if (word == "while")
                {
                    tokens.push_back(Token{ T_WHILE, word, line });
                }
                else
                    tokens.push_back(Token{ T_ID, word, line });
                continue;
            }

            switch (current)
            {
            case '=':
                tokens.push_back(Token{ T_ASSIGN, "=", line });
                break;
            case '+':
                tokens.push_back(Token{ T_PLUS, "+", line });
                break;
            case '-':
                tokens.push_back(Token{ T_MINUS, "-", line });
                break;
            case '*':
                tokens.push_back(Token{ T_MUL, "*", line });
                break;
            case '/':
                tokens.push_back(Token{ T_DIV, "/", line });
                break;
            case '(':
                tokens.push_back(Token{ T_LPAREN, "(", line });
                break;
            case ')':
                tokens.push_back(Token{ T_RPAREN, ")", line });
                break;
            case '{':
                tokens.push_back(Token{ T_LBRACE, "{", line });
                break;
            case '}':
                tokens.push_back(Token{ T_RBRACE, "}", line });
                break;
            case ';':
                tokens.push_back(Token{ T_SEMICOLON, ";", line });
                break;
            case '>':
                tokens.push_back(Token{ T_GT, ">", line });
                break;
            default:
                cout << "Unexpected character: " << current << endl;
                printLineNumber(line);
                exit(1);
            }
            pos++;
        }
        tokens.push_back(Token{ T_EOF, "" });
        return tokens;
    }

    string consumeNumber()
    {
        size_t start = pos;
        while (pos < src.size() && isdigit(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }
};
class SymbolTable
{
public:
    void declareVariable(const string& name, const string& type)
    {
        if (symbolTable.find(name) != symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name +
                "' is already declared.");
        }
        symbolTable[name] = type;
    }

    string getVariableType(const string& name)
    {
        if (symbolTable.find(name) == symbolTable.end())
        {
            throw runtime_error("Semantic error: Variable '" + name +
                "' is not declared.");
        }
        return symbolTable[name];
    }

    bool isDeclared(const string& name) const
    {
        return symbolTable.find(name) != symbolTable.end();
    }

private:
    map<string, string> symbolTable;
};
class IntermediateCodeGnerator
{
public:
    vector<string> instructions;
    int tempCount = 0;
    int labelCount = 0;

    string newTemp() { return "t" + to_string(tempCount++); }
    string newLabel() { return "L" + to_string(labelCount++); }
    void addInstruction(const string& instr) { instructions.push_back(instr); }

    void printInstructions()
    {
        for (const auto& instr : instructions)
        {
            cout << instr << endl;
        }
    }
    vector<string> getInstructions() { return instructions; }
};

class Parser
{
public:
    // Constructor
    Parser(const vector<Token>& tokens, SymbolTable& symTable, IntermediateCodeGnerator& icg)
        : tokens(tokens), pos(0), symTable(symTable), icg(icg)
    {
    }
    // here the private member of this class are being initalized with the
    // arguments passed to this constructor

    void parseProgram()
    {
        while (tokens[pos].type != T_EOF)
        {
            parseStatement();
        }
    }

private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable& symTable;
    IntermediateCodeGnerator& icg;

    void parseStatement()
    {
        if (tokens[pos].type == T_INT)
        {
            parseDeclaration();
        }
        else if (tokens[pos].type == T_ID)
        {
            parseAssignment();
        }
        else if (tokens[pos].type == T_IF)
        {
            parseIfStatement();
        }
        else if (tokens[pos].type == T_WHILE)
        {
            parseWhileLoop();
        }
        else if (tokens[pos].type == T_FOR)
        {
            parseForLoop();
        }
        else if (tokens[pos].type == T_RETURN)
        {
            parseReturnStatement();
        }
        else if (tokens[pos].type == T_LBRACE)
        {
            parseBlock();
        }
        else
        {
            cout << "Syntax error: unexpected token "
                << tokens[pos].value << endl;
            printLineNumber(tokens[pos].lineNumber);
            exit(1);
        }
    }
    void parseWhileLoop()
    {
        expect(T_WHILE);
        expect(T_LPAREN);
        string cond = parseExpression();
        expect(T_RPAREN);

        string startLabel = icg.newLabel();
        string middleLabel = icg.newLabel();
        string endLabel = icg.newLabel();
        string temp = icg.newTemp();

        icg.addInstruction(startLabel + ":");
        icg.addInstruction(temp + " = " + cond);
        icg.addInstruction("if " + temp + " goto " + middleLabel);
        icg.addInstruction("goto " + endLabel);
        icg.addInstruction(middleLabel + ":");
        parseStatement();
        icg.addInstruction("goto " + startLabel);
        icg.addInstruction(endLabel + ":");
    }

    void parseForLoop()
    {
        string temp = icg.newTemp();              
        string condLabel = icg.newLabel();       
        string loopBodyLabel = icg.newLabel();
        string incrementLabel = icg.newLabel(); 
        string endLabel = icg.newLabel();        

        expect(T_FOR);                        
        expect(T_LPAREN);           
        parseDeclarationAndAssignment();    
        icg.addInstruction(condLabel + ":");
        string cond = parseExpression();
        expect(T_SEMICOLON);        
        icg.addInstruction("if " + cond + " goto " + loopBodyLabel);
        icg.addInstruction("goto " + endLabel);
        icg.addInstruction(incrementLabel + ":"); 
        parseAssignment();                  
        expect(T_RPAREN);           
        icg.addInstruction("goto " + condLabel);
        icg.addInstruction(loopBodyLabel + ":");
        parseStatement();                        
        icg.addInstruction("goto " + incrementLabel);
        icg.addInstruction(endLabel + ":");
    }


    void parseDeclarationAndAssignment()
    {
        expect(T_INT);
        string varName = expectAndReturnValue(T_ID);
        symTable.declareVariable(varName, "int");

        if (tokens[pos].type == T_ASSIGN)
        {
            expect(T_ASSIGN);
            string expr = parseExpression();
            icg.addInstruction(varName + " = " + expr);
        }
        expect(T_SEMICOLON);
    }


    void parseDeclaration()
    {
        expect(T_INT); // Expect and consume the int keyword.
        string varName = expectAndReturnValue(
            T_ID); // Expect and return the variable name (identifier).
        symTable.declareVariable(varName,
            "int"); // Register the variable in the symbol
        // table with type "int".
        expect(T_SEMICOLON); // Expect the semicolon to end the statement.
    }


    void parseAssignment()
    {
        string varName = expectAndReturnValue(T_ID);
        symTable.getVariableType(varName); // Ensure the variable is declared in the symbol table.
        expect(T_ASSIGN);
        string expr = parseExpression();
        icg.addInstruction(varName + " = " + expr); // Generate intermediate code for the assignment.
        expect(T_SEMICOLON);
    }

    void parseIfStatement()
    {
        expect(T_IF);
        expect(T_LPAREN); // Expect and consume the opening parenthesis for the
        // condition.
        string cond = parseExpression(); // Parse the condition expression
        // inside the parentheses.
        expect(T_RPAREN);

        string temp = icg.newTemp(); // Generate a new temporary variable for
        // the condition result.
        icg.addInstruction(temp + " = " +
            cond); // Generate intermediate code for storing the
        // condition result.

        icg.addInstruction(
            "if " + temp +
            " goto L1");               // Jump to label L1 if condition is true.
        icg.addInstruction("goto L2"); // Otherwise, jump to label L2.
        icg.addInstruction("L1:");     // Otherwise, jump to label L2.

        parseStatement();

        if (tokens[pos].type ==
            T_ELSE)
        { // If an `else` part exists, handle it.
            icg.addInstruction("goto L3");
            icg.addInstruction("L2:");
            expect(T_ELSE);
            parseStatement(); // Parse the statement inside the else block.
            icg.addInstruction("L3:");
        }
        else
        {
            icg.addInstruction("L2:");
        }
    }

    void parseReturnStatement()
    {
        expect(T_RETURN);
        string expr = parseExpression();
        icg.addInstruction(
            "return " +
            expr); // Generate intermediate code for the return statement.
        expect(T_SEMICOLON);
    }

    void parseBlock()
    {
        expect(T_LBRACE); // Expect and consume the opening brace `{`.
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement(); // Parse the statements inside the block.
        }
        expect(T_RBRACE);
    }

    string parseExpression()
    {
        string term = parseTerm();
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS)
        {
            TokenType op = tokens[pos++].type;
            string nextTerm =
                parseTerm(); // Parse the next term in the expression.
            string temp =
                icg.newTemp(); // Generate a temporary variable for the result
            icg.addInstruction(temp + " = " + term +
                (op == T_PLUS ? " + " : " - ") +
                nextTerm); // Intermediate code for operation
            term = temp;
        }
        if (tokens[pos].type == T_GT)
        {
            pos++;
            string nextExpr = parseExpression(); // Parse the next expression
            // for the comparison.
            string temp =
                icg.newTemp(); // Generate a temporary variable for the result.
            icg.addInstruction(
                temp + " = " + term + " > " +
                nextExpr); // Intermediate code for the comparison.
            term = temp;
        }
        return term;
    }

    string parseTerm()
    {
        string factor = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV)
        {
            TokenType op = tokens[pos++].type;
            string nextFactor = parseFactor();
            string temp =
                icg.newTemp(); // Generate a temporary variable for the result.
            icg.addInstruction(temp + " = " + factor +
                (op == T_MUL ? " * " : " / ") +
                nextFactor); // Intermediate code for operation.
            factor = temp;                  // Update the factor to be the temporary result.
        }
        return factor;
    }

    string parseFactor()
    {
        if (tokens[pos].type == T_NUM)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_ID)
        {
            return tokens[pos++].value;
        }
        else if (tokens[pos].type == T_LPAREN)
        {
            expect(T_LPAREN);
            string expr = parseExpression();
            expect(T_RPAREN);
            return expr;
        }
        else
        {
            cout << "Syntax error: unexpected token '" << tokens[pos].value
                << "' at line " << tokens[pos].lineNumber << endl;
            exit(1);
        }
    }

    void expect(TokenType type)
    {
        if (tokens[pos].type != type)
        {
            cout << "Syntax error: expected '" << type << "' at line "
                << tokens[pos].lineNumber << endl;
            exit(1);
        }
        pos++;
    }

    string expectAndReturnValue(TokenType type)
    {
        string value = tokens[pos].value;
        expect(type);
        return value;
    }


};

class AssemblyGenerator
{
public:
    vector<string> assemblyInstructions;
    unordered_map<string, string> registerMap;

    string getRegister(const string& var)
    {
        if (registerMap.find(var) == registerMap.end())
        {
            registerMap[var] = "R" + to_string(registerMap.size());
        }
        return registerMap[var];
    }
    void generateAssembly(const vector<string>& threeAddressCode)
    {
        for (const auto& instr : threeAddressCode)
        {
            vector<string> tokens = tokenize(instr);
            if (tokens.size() == 3 && tokens[1] == "=")
            {
                if (isdigit(tokens[2][0]))
                {
                    assemblyInstructions.push_back("MOV " + tokens[0] + " , " + tokens[2]);
                }
                else
                {
                    string reg = getRegister(tokens[2]);
                    assemblyInstructions.push_back("MOV " + reg + ", " + tokens[2]);
                    assemblyInstructions.push_back("MOV " + tokens[0] + ", " + reg);
                }
            }
            else if (tokens.size() == 4) {
                assemblyInstructions.push_back("CMP " + tokens[1] + " , 0");
                assemblyInstructions.push_back("JNE " + tokens[3]);
            }
            else if (tokens.size() == 2) {
                if(tokens[0] == "return"){
                    string reg = getRegister(tokens[1]);
                    assemblyInstructions.push_back("MOV " + reg + " , " + tokens[1]);
                    assemblyInstructions.push_back("JMP END");
                }
                assemblyInstructions.push_back("JMP " + tokens[1]);

            }
            else if (tokens.size() == 5)
            {
                if (tokens[3] == ">")
                {
                    assemblyInstructions.push_back("CMP " + tokens[2] + " , " + tokens[4]);
                    assemblyInstructions.push_back("SETG AL");
                    assemblyInstructions.push_back("MOV " + tokens[0] + " , AL");
                    continue;
                }
                string reg1 = getRegister(tokens[2]);
                assemblyInstructions.push_back("MOV " + reg1 + ", " + tokens[2]);
                if (tokens[3] == "+")
                {
                    assemblyInstructions.push_back("ADD " + reg1 + " , " + tokens[4]);
                }
                else if (tokens[3] == "-")
                {
                    assemblyInstructions.push_back("SUB " + reg1 + " , " + tokens[4]);
                }
                else if (tokens[3] == "*")
                {
                    assemblyInstructions.push_back("MUL " + reg1 + " , " + tokens[4]);
                }
                else if (tokens[3] == "/")
                {
                    assemblyInstructions.push_back("DIV " + reg1 + " , " + tokens[4]);
                }
                assemblyInstructions.push_back("MOV " + tokens[0] + " , " + reg1);
            }
            else if (tokens.size() == 1 && tokens[0].back() == ':')
            {
                assemblyInstructions.push_back(tokens[0]);
            }
            else
            {
                assemblyInstructions.push_back("Unexpected instruction: " + instr);
            }
        }
        assemblyInstructions.push_back("END:");
    }

    void printAssembly()
    {
        for (const auto& instr : assemblyInstructions)
        {
            cout << instr << endl;
        }
    }

private:
    vector<string> tokenize(const string& str)
    {
        vector<string> tokens;
        string token;
        istringstream tokenStream(str);
        while (getline(tokenStream, token, ' '))
        {
            tokens.push_back(token);
        }
        return tokens;
    }
};
int main()
{
    string input = R"(
        int a;
        // a = b;
        // a = 5;
        int b;
        // if(a>b){}

        // b = a + 10;
        // if (b > 10)
        // {
        //     return a;
        // } else {
        //     return 0;
            
        // }
        for( int x = 1 + 3;
        a > b ; 
         a = a + 1;){
            a = a + 1;
        }
        // a = 12;
        // while(a > b){
        // a = b;
        //     }
        // a = 13;

    )";

    Lexer lexer(input);
    vector<Token> tokens = lexer.tokenize();

    SymbolTable symTable;
    IntermediateCodeGnerator icg;
    Parser parser(tokens, symTable, icg);
    parser.parseProgram();
    icg.printInstructions();
    vector<string> tac = icg.getInstructions();

    AssemblyGenerator ag;
    ag.generateAssembly(tac);
    ag.printAssembly();

    return 0;
}
