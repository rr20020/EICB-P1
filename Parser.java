/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import jdk.internal.loader.AbstractClassLoaderValue;
import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import javax.xml.transform.Source;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.locks.Condition;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
   Marek Daniv 2558754
   Rizwana Rathmann 1309360
		

 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(RECORD);
		String name = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elements = new ArrayList<>();
		// no empty records allowed
		elements.add(parseRecordElementDeclaration());
		while(currentToken.type != RBRACE) {
			elements.add(parseRecordElementDeclaration());
		}
		accept(RBRACE);
		
		return new RecordTypeDeclaration(location, name, elements);
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				acceptIt();
				isVariable = false;
				break;
			case VAR:
				acceptIt();
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		
		return new RecordElementDeclaration(location, isVariable, typeSpecifier, name);
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		TypeSpecifier tspc;
		String name;
		switch (currentToken.type){
			case VAL:
			acceptIt();
			tspc = parseTypeSpecifier();
			 name = accept(ID);
			return new IteratorDeclaration(location,name,tspc,false);

			case VAR:
			acceptIt();
			tspc = parseTypeSpecifier();
			 name = accept(ID);
			return new IteratorDeclaration(location,name,tspc,true);

			default: throw new SyntaxError(currentToken,VAR,VAL);
	}

	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); // "Val" akzeptieren
		TypeSpecifier typeSpecifier = parseTypeSpecifier(); // 	public ValueDefinition(SourceLocation sourceLocation, TypeSpecifier typeSpecifier, String valueName, Expression value) {
		String name = accept(ID);
		accept(ASSIGN);
		Expression expr = parseExpr();
		accept(SEMICOLON);

		return new ValueDefinition(location,typeSpecifier,name,expr);
	}
	
	private VariableDeclaration parseVarDecl() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); //" Var" akzeptieren
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);

		return new VariableDeclaration(location,typeSpecifier,name);
	}
	
	private ReturnStatement parseReturn() {

		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); //accept Return
		Expression result = parseExpr();
		accept(SEMICOLON);

		return new ReturnStatement(location,result);

	}
	
	private Statement parseAssignOrCall() {
		SourceLocation location = currentToken.sourceLocation;
		
		String name = accept(ID);
		
		Statement s;
		if(currentToken.type != LPAREN)
			s = parseAssign(name, location);
		else
			s = new CallStatement(location, parseCall(name, location));
		accept(SEMICOLON);
		
		return s;
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		//ID identifiziert
		String nameRecordAttribut ;
		Expression firstIndexExpr;
		Expression secondIndexExpr;

		LeftHandIdentifier idf;

		switch(currentToken.type){
			case AT: acceptIt();   nameRecordAttribut = accept(ID);
				idf = new RecordLhsIdentifier(location,name,nameRecordAttribut);

			break;
			case LBRACKET: acceptIt();
						   firstIndexExpr = parseExpr();
						   accept(RBRACKET);

			              if(currentToken.type == LBRACKET){
			              	acceptIt();
			              	secondIndexExpr = parseExpr();
			              	accept(RBRACKET);
			              	idf = new MatrixLhsIdentifier(location,name,firstIndexExpr,secondIndexExpr);break;
			              }

			              idf = new VectorLhsIdentifier(location,name,firstIndexExpr);
			default: idf = new LeftHandIdentifier(location,name);
		}

		accept(ASSIGN);
		Expression rightHandExpr = parseExpr();

		return new VariableAssignment(location,idf,rightHandExpr);
	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		acceptIt(); // accept lparen

		List<Expression> paramList = new ArrayList<>();


		while(currentToken.type !=RPAREN){
			paramList.add(parseExpr());
			if(currentToken.type != COMMA)//Nur ein Argument oder fehlerhafte Eingabe -> weiterreichen
				break;
			acceptIt();
		}

		accept(RPAREN); //RParen accepten
		return new CallExpression(location,name,paramList);

	}
	
	private ForLoop parseFor() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(FOR);
		accept(LPAREN);
		String name = accept(ID);
		accept(ASSIGN);
		Expression a = parseExpr();
		accept(SEMICOLON);
		Expression b = parseExpr();
		accept(SEMICOLON);
		String inc = accept(ID);
		accept(ASSIGN);
		Expression c = parseExpr();
		accept(RPAREN);
		return new ForLoop(location, name, a, b, inc, c, parseStatement());
	}
	
	private ForEachLoop parseForEach() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt();//"foreach" accepten
		accept(LPAREN);
		//Parse Iterator
		IteratorDeclaration itDec = parseIteratorDeclaration();

		accept(COLON);
		Expression expr = parseExpr();
		accept(RPAREN);

		Statement stmt = parseStatement();

		return new ForEachLoop(location,itDec,expr,stmt);
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); // IF
		accept( LPAREN);
		Expression ifCondition = parseExpr();
		accept(RPAREN);

		Statement ifStatement = parseStatement(); // Compound enthalten

		if(currentToken.type == ELSE ) {
			acceptIt();
			Statement elseStatement = parseStatement();
			return new IfStatement(location,ifCondition,ifStatement,elseStatement);
		}

		return new IfStatement(location,ifCondition,ifStatement);
	}
	
	private SwitchStatement parseSwitch() {

		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); // "switch akkzeptieren"

		accept(LPAREN);
		Expression switchCond = parseExpr();
		accept(RPAREN);

		accept(LBRACE);

		List<Case> cases = new ArrayList<>();
		List<Default> defaultCases = new ArrayList<>();

		while(currentToken.type != RBRACE) {

			switch (currentToken.type) {
				case CASE:
					cases.add(parseCase());break;

				case DEFAULT:
				 defaultCases.add(parseDefault()) ;break;

				default:	throw new SyntaxError(currentToken,CASE,DEFAULT);
			}
		}

		accept(RBRACE);
		return new SwitchStatement(location,switchCond,cases,defaultCases);
	}
	
	private Case parseCase() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); // parse "Case"
		Expression caseCond = parseExpr();
		accept(COLON);

		Statement stmt = parseStatement();

		return new Case(location,caseCond,stmt);
	}
	
	private Default parseDefault() {
		SourceLocation location = currentToken.sourceLocation;

		acceptIt(); // parse "Default"
		accept(COLON);

		Statement stmt = parseStatement();

		return new Default(location,stmt);
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		acceptIt(); //  LBRACE

		List<Statement> statements = new ArrayList<>(); // Stmt Liste initialisieren

		while(currentToken.type != RBRACE ){ // Solange bist ende des Compound stmt erreicht ist

			statements.add(parseStatement());

		}

		accept(RBRACE);
		return new CompoundStatement(location,statements);


	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt(); // "!" einlesen

			return new Not(location, parseCompare());
		}
		return parseCompare();
	}
	
	private Expression parseCompare() {

		SourceLocation location = currentToken.sourceLocation;

		Expression leftExpr = parseAddSub(); // Parse linke Seite des nächsten Vergleichs bzw. die rechteste Expr des gesamten Ausdrucks
		while(currentToken.type == LANGLE || currentToken.type == RANGLE
				|| currentToken.type == CMPLE || currentToken.type == CMPGE
				|| currentToken.type == CMPEQ || currentToken.type == CMPNE) {
			Expression rightExpression;
			switch (currentToken.type) {

				case LANGLE:
					acceptIt();
					rightExpression = parseAddSub();
					leftExpr = new Compare(location, leftExpr, rightExpression, LESS); break;

				case CMPLE:
					acceptIt();
					rightExpression = parseAddSub();
					leftExpr= new Compare(location, leftExpr, rightExpression, LESS_EQUAL);break;

				case RANGLE:
					acceptIt(); // größerGleich Comparison
					rightExpression = parseAddSub();

					leftExpr = new Compare(location, leftExpr, rightExpression, GREATER);break;

				case CMPGE:
					acceptIt();
					rightExpression = parseAddSub();
					leftExpr = new Compare(location, leftExpr, rightExpression, GREATER_EQUAL);break;

				case CMPEQ:
					acceptIt(); // Gleich Comparison
					rightExpression = parseAddSub();
					leftExpr = new Compare(location, leftExpr, rightExpression, EQUAL);break;

				case CMPNE:
					acceptIt(); // nichtGleich Comparison
					rightExpression = parseAddSub();
					leftExpr  =new Compare(location, leftExpr, rightExpression, NOT_EQUAL);break;


			}
		}
		return leftExpr;

	}

	
	private Expression parseAddSub() {
		SourceLocation location = currentToken.sourceLocation;

		Expression leftExpr = parseMulDiv();
		Expression rightExpr;

		while(currentToken.type == ADD || currentToken.type == SUB) {

			switch (currentToken.type) {

				case ADD:
					acceptIt();
					rightExpr = parseMulDiv();

					leftExpr= new Addition(location, leftExpr, rightExpr);break;

				case SUB:
					acceptIt();
					rightExpr = parseMulDiv();

					leftExpr = new Subtraction(location, leftExpr, rightExpr);break;


			}
		}

		return leftExpr;
	}
	
	private Expression parseMulDiv() {
		SourceLocation location = currentToken.sourceLocation;

		Expression leftExpr = parseUnaryMinus();
		Expression rightExpr ;

		while(currentToken.type == MULT || currentToken.type == DIV) {
			switch (currentToken.type) {
				case MULT:
					acceptIt();
					rightExpr = parseUnaryMinus();

					leftExpr= new Multiplication(location, leftExpr, rightExpr);break;

				case DIV:
					acceptIt();
					rightExpr = parseUnaryMinus();
					leftExpr = new Division(location, leftExpr, rightExpr);break;


			}
		}

		return leftExpr;
	}
	
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			Expression  exponentiation = parseExponentiation();
			return new UnaryMinus(location, exponentiation);
		}
		return parseExponentiation();
	}
	
	private Expression parseExponentiation() {

		SourceLocation location = currentToken.sourceLocation;
        Expression Expr = parseDotProd();


        while(currentToken.type == EXP ){
        	acceptIt();
        	Expr = new Exponentiation(location, Expr , parseExponentiation());

		}

        return Expr;
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			return new MatrixTranspose(location, parseDim());
		}
		return parseDim();
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}
	
	private Expression parseElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseRecordElementSelect();
		
		while(currentToken.type == LBRACKET) {
			acceptIt();
			Expression idx = parseExpr();
			accept(RBRACKET);
			x = new ElementSelect(location, x, idx);
		}
		
		return x;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
