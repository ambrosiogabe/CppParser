#ifndef GABE_CPP_PARSER_AST
#define GABE_CPP_PARSER_AST

namespace CppParser
{
	struct AstNode;
	struct PreprocessingAstNode;
	enum class PreprocessingAstNodeType
	{
		None,
		PreprocessingOpOrPunc,
		PreprocessingFile,
		Group,
		IfSection,
		IfGroup,
		IfDefGroup,
		IfNDefGroup,
		ElifGroups,
		ElifGroup,
		ElseGroup,
		MacroInclude,
		MacroDefine,
		MacroDefineFunction,
		MacroUndef,
		MacroLine,
		MacroError,
		MacroPragma,
		TextLine,
		NonDirective,
		Identifier,
		IdentifierList,
		ReplacementList,
		PPTokens,
		NumberLiteral,
		StringLiteral,
		CharacterLiteral,
		HeaderName,
		HeaderNameString,
		EmptyMacro,
		Newline,
		Whitespace,
		All,
	};

	enum class AstNodeType
	{
		None,
		All,
		BracedInitList,
		InitializerClause,
		InitializerList,
		OperatorFunctionId,
		BinaryExpression,
		TernaryExpression,
		AssignmentExpression,
		Expression,
		PointerToMember,
		CastExpression,
		UnaryExpression,
		SizeofExpression,
		SizeofIdentifierExpression,
		AlignofExpression,
		Delete,
		Literal,
		This,
		Grouping,
		LambdaExpression,
		LambdaIntroducer,
		LambdaCapture,
		Capture,
		LambdaCaptureList,
		LambdaDeclarator,
		TemplateArgumentList,
		TemplateQualifiedId,
		TypeId,
		EnumName,
		EnumSpecifier,
		EnumKey,
		EnumHead,
		OpaqueEnumDecl,
		EnumBase,
		EnumeratorList,
		EnumeratorDefinition,
		ConstantExpression,
		IfElse,
		Switch,
		InitializerCondition,
		BracedInitCondition,
		WhileLoop,
		DoWhileLoop,
		ForLoop,
		ForEachLoop,
		ForRangeDeclaration,
		ForRangeInitializer,
		Statement,
		QualifiedId,
		LabeledIdentifier,
		CaseLabel,
		DefaultLabel,
		CompoundStatement,
		StatementSequence,
		Break,
		Continue,
		Return,
		Goto,
		NamespaceName,
		NamedNamespaceDefinition,
		UnnamedNamespaceDefinition,
		NamespaceAliasDefinition,
		QualifiedNamespaceSpecifier,
		UsingDeclaration,
		UsingTypenameDeclaration,
		UsingDirective,
		Asm,
		LinkageSpecificationBlock,
		LinkageSpecification,
		AttributeSpecifierSequence,
		AlignAsTypeId,
		AlignAsExpression,
		AttributeList,
		EmptyAttributeList,
		Attribute,
		AttributeToken,
		AttributeArgumentClause,
		BalancedTokenSeq,
		BalancedToken,
		InitDeclaratorList,
		InitDeclarator,
		Declarator,
		PtrDeclarator,
		ParametersAndQualifiers,
		TrailingReturnType,
		PtrStar,
		Ref,
		RefRef,
		PtrNamespaceStar,
		CvQualifierSeq,
		CvQualifier,
		RefQualifier,
		DeclaratorId,
		ClassName,
		ClassSpecifier,
		ClassHead,
		ClassVirtualHead,
		ClassHeadName,
		ClassVirtSpecifierSeq,
		ClassVirtSpecifier,
		ClassKey,
		MemberAndAccessSpecifier,
		MemberSpecifier,
		MemberFunctionDeclaration,
		MemberDeclaration,
		MemberDeclaratorList,
		MemberDeclaratorPure,
		MemberDeclaratorBrace,
		MemberDeclarator,
		VirtSpecifierSeq,
		VirtSpecifier,
		PureSpecifier,
		BaseSpecifierList,
		BaseSpecifier,
		ClassOrDecltype,
		AccessSpecifier,
		ConversionFunctionId,
		ConversionTypeId,
		ConversionDeclarator,
		MemInitializerList,
		MemExpressionInitializer,
		MemBracedInitializer,
		MemInitializerId,
		NestedNamespaceSpecifierId,
		NestedNamespaceSpecifierTemplate,
		PostfixSimpleTypeExpressionList,
		PostfixSimpleTypeBraceList,
		PostfixTypenameSpecExpressionList,
		PostfixTypenameSpecBraceList,
		PostfixCast,
		PostfixTypeIdExpression,
		PostfixTypeId,
		PostfixBracketExpression,
		PostfixBracketBraceList,
		PostfixParenExpressionList,
		PostfixMemberIdExpression,
		PostfixPseudoDestructor,
		PostfixPlusPlus,
		PostfixMinusMinus,
		PseudoDestructorDecltype,
		PseudoDestructorTemplate,
		PseudoDestructor,
		PseudoNestedDestructor,
		NewTypeIdExpression,
		NewExpression,
		NewPlacement,
		NewTypeId,
		NewDeclarator,
		NoptrNewTailDeclarator,
		NoptrNewDeclarator,
		NewInitializer,
		DeclarationSeq,
		AliasDeclaration,
		SimpleDeclaration,
		StaticAssertDeclaration,
		SimpleDeclSpecifier,
		DeclSpecifier,
		DeclSpecSeq,
		StorageClassSpec,
		FunctionSpec,
		TypedefName,
		TypeSpecSeq,
		TrailingTypeSpecSeq,
		SimpleTypeTokenSpec,
		SimpleTypeTemplateSpec,
		SimpleTypeSpec,
		DecltypeSpec,
		AbstractDeclarator,
		AbstractElipsisDeclarator,
		PtrAbstractDeclarator,
		ParameterDeclarationList,
		ParameterDefaultDeclaration,
		ParameterDeclaration,
		ParameterAbstractDefaultDeclaration,
		ParameterAbstractDeclaration,
		FunctionDefaultDefinition,
		FunctionDefinition,
		FunctionBody,
		LiteralOperatorId,
		TemplateDeclaration,
		TemplateParameterList,
		SimpleTemplateId,
		LiteralOperatorTemplateId,
		FunctionOperatorTemplateId,
		TemplateName,
		TypenameSpecifier,
		TypenameTemplateSpecifier,
		ExplicitInstantiation,
		TryBlock,
		FunctionTryBlock,
		HandlerSeq,
		Handler,
		ExceptionDeclaration,
		ExceptionAbstractDeclaration,
		ThrowExpression,
		DynamicExceptionSpec,
		TypeIdList,
		NoexceptSpec,
		NoexceptExpressionSpec,
		NoptrAbstractDeclarator,
		NoptrAbstractExpressionDeclarator,
		UnqualifiedId,
		UnqualifiedIdDtorClass,
		UnqualifiedIdDtorDecltype,
		ElaboratedSpecifierEnum,
		ElaboratedSpecifierClass,
		ElaboratedSpecifierTemplate,
		AlignmentExpression,
		NoPtrParenDeclarator,
		NoPtrBracketDeclarator,
		NoPtrParamAndQualDeclarator,
		NoPtrDeclarator,
		USystemDeclaration,
		TypeTemplateParameter,
		TypeTypenameParameter,
		TypeClassParameter,
		Empty,
	};

	enum class OverloadableOperatorType
	{
		None,
		New,
		Delete,
		NewArr,
		DeleteArr,
		Plus,
		Minus,
		Multiply,
		Divide,
		Modulo,
		Xor,
		BitAnd,
		BitOr,
		BitComplement,
		Not,
		Assign,
		LessThan,
		GreaterThan,
		PlusEqual,
		MinusEqual,
		MultiplyEqual,
		DivideEqual,
		ModuloEqual,
		CaretEqual,
		BitAndEqual,
		BitOrEqual,
		LeftShift,
		RightShift,
		RightShiftEqual,
		LeftShiftEqual,
		EqualEqual,
		NotEqual,
		LessThanEqual,
		GreaterThanEqual,
		LogicAnd,
		LogicOr,
		PlusPlus,
		MinusMinus,
		Comma,
		ArrowStar,
		Arrow,
		ParenGroup,
		AddressOf,
		Dereference,
		Negative,
		Positive,
		BracketGroup
	};
	enum class AssignmentOperatorType
	{
		None,
		Equal,
		TimesEqual,
		DivEqual,
		ModEqual,
		PlusEqual,
		MinusEqual,
		RightShiftEqual,
		LeftShiftEqual,
		AndEqual,
		XorEqual,
		OrEqual
	};
	enum class EnumKeyType
	{
		Enum,
		Class,
		Struct
	};
	enum class CastType
	{
		DynamicCast,
		ReinterpretCast,
		StaticCast,
		ConstCast,
	};
	enum class MemberOperatorType
	{
		DotOperator,
		ArrowOperator
	};
	enum class AutoFunctionType
	{
		Default,
		Delete
	};

	struct TypeClassParameterNode
	{
		Token identifier;
		AstNode* typeId;
	};

	struct TypeTypenameParameterNode
	{
		Token identifier;
		AstNode* typeId;
	};

	struct TypeTemplateParameterNode
	{
		AstNode* templateParameterList;
		Token identifier;
		AstNode* idExpression;
	};

	struct InitializerListNode
	{
		AstNode* thisInitList;
		AstNode* nextInitList;
	};

	struct USystemDeclarationNode
	{
		AstNode* parameterDeclarationClause;
		AstNode* namedNamespaceDefinition;
	};

	struct NoPtrParenDeclaratorNode
	{
		AstNode* ptrDeclarator;
	};

	struct NoPtrBracketDeclaratorNode
	{
		AstNode* constantExpression;
		AstNode* attributeSpecifierSeq;
		AstNode* noptrDeclarator;
	};

	struct NoPtrParamAndQualDeclaratorNode
	{
		AstNode* parametersAndQualifiers;
		AstNode* noptrDeclarator;
	};

	struct NoPtrDeclaratorNode
	{
		AstNode* declaratorId;
		AstNode* attributeSpecifierSeq;
	};

	struct AlignmentExpressionNode
	{
		AstNode* typeId;
	};

	struct ElaboratedSpecifierClassNode
	{
		AstNode* classKey;
		AstNode* attributeSpecifierSeq;
		AstNode* nestedNameSpecifier;
		Token identifier;
		bool hasScopeOp;
	};

	struct ElaboratedSpecifierTemplateNode
	{
		AstNode* classKey;
		AstNode* nestedNameSpecifier;
		AstNode* simpleTemplateId;
		bool hasScopeOp;
		bool hasTemplateKeyword;
	};

	struct ElaboratedSpecifierEnumNode
	{
		AstNode* nestedNameSpecifier;
		Token identifier;
		bool hasScopeOp;
	};

	struct UnqualifiedIdDtorDecltypeNode
	{
		AstNode* decltypeSpecifier;
	};

	struct UnqualifiedIdDtorClassNode
	{
		AstNode* className;
	};

	struct UnqualifiedIdNode
	{
		Token identifier;
	};

	struct NoptrAbstractExpressionDeclaratorNode
	{
		AstNode* ptrAbstractDeclarator;
		AstNode* constantExpression;
		AstNode* attributeSpecifierSeq;
		AstNode* noptrAbstractDeclarator;
	};

	struct NoptrAbstractDeclaratorNode
	{
		AstNode* ptrAbstractDeclarator;
		AstNode* parametersAndQualifiers;
		AstNode* noptrAbstractDeclarator;
	};

	struct NoexceptExpressionNode
	{
		AstNode* constantExpression;
	};

	struct TypeIdListNode
	{
		AstNode* thisTypeId;
		AstNode* nextTypeId;
	};

	struct DynamicExceptionSpecNode
	{
		AstNode* typeIdList;
	};

	struct ThrowExpressionNode
	{
		AstNode* assignmentExpression;
	};

	struct ExceptionAbstractDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* typeSpecifierSeq;
		AstNode* abstractDeclarator;
	};

	struct ExceptionDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* typeSpecifierSeq;
		AstNode* declarator;
	};

	struct HandlerNode
	{
		AstNode* exceptionDeclaration;
		AstNode* compoundStatement;
	};

	struct HandlerSeqNode
	{
		AstNode* thisHandler;
		AstNode* nextHandler;
	};

	struct FunctionTryBlockNode
	{
		AstNode* ctorInitializer;
		AstNode* compoundStatement;
		AstNode* handlerSeq;
	};

	struct TryBlockNode
	{
		AstNode* compoundStatement;
		AstNode* handlerSeq;
	};

	struct ExplicitInstantiationNode
	{
		AstNode* declaration;
		bool hasExternKeyword;
	};

	struct TypenameSpecifierNode
	{
		AstNode* nestedNameSpecifier;
		Token identifier;
	};

	struct TypenameTemplateSpecifierNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* simpleTemplateId;
		bool hasTemplateKeyword;
	};

	struct TemplateNameNode
	{
		Token identifier;
	};

	struct FunctionOperatorTemplateIdNode
	{
		AstNode* operatorFunctionId;
		AstNode* templateArgumentList;
	};

	struct LiteralOperatorTemplateIdNode
	{
		AstNode* literalOperatorId;
		AstNode* templateArgumentList;
	};

	struct SimpleTemplateIdNode
	{
		AstNode* templateName;
		AstNode* templateArgumentList;
	};

	struct TemplateParameterListNode
	{
		AstNode* thisParameter;
		AstNode* nextParameter;
	};

	struct TemplateDeclarationNode
	{
		AstNode* templateParameterList;
		AstNode* declaration;
	};

	struct LiteralOperatorIdNode
	{
		Token identifier;
	};

	struct FunctionBodyNode
	{
		AstNode* ctorInitializer;
		AstNode* compoundStatement;
	};

	struct FunctionDefinitionNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
		AstNode* functionBody;
	};

	struct FunctionDefaultDefinitionNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
		AutoFunctionType functionType;
	};

	struct ParameterAbstractDefaultDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* abstractDeclarator;
		AstNode* initializerClause;
	};

	struct ParameterAbstractDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* abstractDeclarator;
	};

	struct ParameterDefaultDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
		AstNode* initializerClause;
	};

	struct ParameterDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
	};

	struct ParameterDeclarationListNode
	{
		AstNode* thisParameter;
		AstNode* nextParameter;
	};

	struct PtrAbstractDeclaratorNode
	{
		AstNode* ptrOperator;
		AstNode* ptrAbstractDeclarator;
	};

	struct AbstractDeclaratorNode
	{
		AstNode* noptrAbstractDeclarator;
		AstNode* parametersAndQualifiers;
		AstNode* trailingReturnType;
	};

	struct DecltypeSpecNode
	{
		AstNode* expression;
	};

	struct SimpleTypeTokenSpecNode
	{
		Token type;
	};

	struct SimpleTypeTemplateSpecNode
	{
		AstNode* nestedNameSpec;
		AstNode* simpleTemplateId;
	};

	struct SimpleTypeSpecNode
	{
		AstNode* nestedNameSpec;
		AstNode* typeName;
	};

	struct TrailingTypeSpecSeqNode
	{
		AstNode* thisTypeSpec;
		AstNode* nextTypeSpec;
		AstNode* attributeSpecifierSeq;
	};

	struct TypeSpecSeqNode
	{
		AstNode* thisTypeSpec;
		AstNode* nextTypeSpec;
		AstNode* attributeSpecifierSeq;
	};

	struct TypedefNameNode
	{
		Token identifier;
	};

	struct FunctionSpecNode
	{
		Token specifier;
	};

	struct StorageClassSpecNode
	{
		Token specifier;
	};

	struct DeclSpecSeqNode
	{
		AstNode* thisSpec;
		AstNode* nextSpec;
		AstNode* attributeSpecifierSeq;
	};

	struct SimpleDeclSpecifierNode
	{
		Token token;
	};

	struct DeclSpecifierNode
	{
		AstNode* specifier;
	};

	struct StaticAssertDeclarationNode
	{
		AstNode* constantExpression;
		Token stringLiteral;
	};

	struct SimpleDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* initDeclaratorList;
	};

	struct AliasDeclarationNode
	{
		Token identifier;
		AstNode* typeId;
	};

	struct DeclarationSeqNode
	{
		AstNode* thisDeclaration;
		AstNode* nextDeclaration;
	};

	struct NewInitializerNode
	{
		AstNode* expressionList;
	};

	struct NoptrNewTailDeclaratorNode
	{
		AstNode* expression;
		AstNode* attributeSpecifierSeq;
	};

	struct NoptrNewDeclaratorNode
	{
		AstNode* noptrNewDeclarator;
		AstNode* constantExpression;
		AstNode* attributeSpecifierSeq;
	};

	struct NewDeclaratorNode
	{
		AstNode* ptrOperator;
		AstNode* newDeclarator;
	};

	struct NewTypeIdNode
	{
		AstNode* typeSpecifierSeq;
		AstNode* newDeclarator;
	};

	struct NewPlacementNode
	{
		AstNode* expressionList;
	};

	struct NewExpressionNode
	{
		AstNode* newPlacement;
		AstNode* typeId;
		AstNode* newInitializer;
	};

	struct NewTypeIdExpressionNode
	{
		AstNode* newPlacement;
		AstNode* newTypeId;
		AstNode* newInitializer;
	};

	struct PseudoDestructorNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* typeName;
	};

	struct PseudoNestedDestructorNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* nestedTypeName;
		AstNode* typeName;
	};

	struct PseudoDestructorTemplateNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* simpleTemplateId;
		AstNode* typeName;
	};

	struct PseudoDestructorDecltypeNode
	{
		AstNode* decltypeSpecifier;
	};

	struct PostfixPlusPlusNode
	{
		AstNode* postfixExpression;
	};

	struct PostfixMinusMinusNode
	{
		AstNode* postfixExpression;
	};

	struct PostfixPseudoDestructorNode
	{
		AstNode* postfixExpression;
		AstNode* pseudoDestructorName;
		MemberOperatorType memberOp;
	};

	struct PostfixMemberIdExpressionNode
	{
		AstNode* postfixExpression;
		AstNode* idExpression;
		bool hasTemplateKeyword;
		MemberOperatorType memberOp;
	};

	struct PostfixParenExpressionListNode
	{
		AstNode* postfixExpression;
		AstNode* expressionList;
	};

	struct PostfixBracketBraceListNode
	{
		AstNode* postfixExpression;
		AstNode* bracedInitList;
	};

	struct PostfixBracketExpressionNode
	{
		AstNode* postfixExpression;
		AstNode* expression;
	};

	struct PostfixTypeIdExpressionNode
	{
		AstNode* expression;
	};

	struct PostfixTypeIdNode
	{
		AstNode* typeId;
	};

	struct PostfixCastNode
	{
		AstNode* typeId;
		AstNode* expression;
		CastType castType;
	};

	struct PostfixTypenameSpecExpressionListNode
	{
		AstNode* typenameSpecifier;
		AstNode* expressionList;
	};

	struct PostfixTypenameSpecBraceListNode
	{
		AstNode* typenameSpecifier;
		AstNode* bracedInitList;
	};

	struct PostfixSimpleTypeExpressionListNode
	{
		AstNode* simpleTypeSpecifier;
		AstNode* expressionList;
	};

	struct PostfixSimpleTypeBraceListNode
	{
		AstNode* simpleTypeSpecifier;
		AstNode* bracedInitList;
	};

	struct NestedNamespaceSpecifierTemplateNode
	{
		AstNode* nestedNameSpecifier;
		bool hasTemplateKeyword;
		AstNode* simpleTemplateId;
	};

	struct NestedNamespaceSpecifierIdNode
	{
		AstNode* nestedNameSpecifier;
		Token identifier;
	};

	struct MemInitializerIdNode
	{
		Token identifier;
	};

	struct MemBracedInitializerNode
	{
		AstNode* memInitializerId;
		AstNode* bracedInitList;
	};

	struct MemExpressionInitializerNode
	{
		AstNode* memInitializerId;
		AstNode* expressionList;
	};

	struct MemInitializerListNode
	{
		AstNode* thisMemInitializer;
		AstNode* nextMemInitializer;
	};

	struct ConversionDeclaratorNode
	{
		AstNode* ptrOperator;
		AstNode* conversionDeclarator;
	};

	struct ConversionTypeIdNode
	{
		AstNode* typeSpecifierSeq;
		AstNode* conversionDeclarator;
	};

	struct ConversionFunctionIdNode
	{
		AstNode* conversionTypeId;
	};

	struct AccessSpecifierNode
	{
		Token accessSpecifier;
	};

	struct ClassOrDecltypeNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* className;
	};

	struct BaseSpecifierNode
	{
		AstNode* attributeSpecifierSeq;
		bool isVirtual;
		AstNode* accessSpecifier;
		AstNode* baseTypeSpecifier;
	};

	struct BaseSpecifierListNode
	{
		AstNode* thisBaseSpecifier;
		AstNode* nextBaseSpecifier;
	};

	struct VirtSpecifierNode
	{
		Token token;
	};

	struct VirtSpecifierSeqNode
	{
		AstNode* thisSpec;
		AstNode* nextSpec;
	};

	struct MemberDeclaratorNode
	{
		Token identifier;
		AstNode* attributeSpecifierSeq;
		AstNode* virtSpecifierSeq;
		AstNode* constantExpression;
	};

	struct MemberDeclaratorBraceNode
	{
		AstNode* declarator;
		AstNode* virtSpecifierSeq;
		AstNode* braceOrEqualInitializer;
	};

	struct MemberDeclaratorPureNode
	{
		AstNode* declarator;
		AstNode* virtSpecifierSeq;
		AstNode* pureSpecifier;
	};

	struct MemberDeclaratorListNode
	{
		AstNode* thisDeclarator;
		AstNode* nextDeclarator;
	};

	struct MemberDeclarationNode
	{
		AstNode* attribtueSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* memberDeclaratorList;
	};

	struct MemberFunctionDeclarationNode
	{
		AstNode* functionDefinition;
		bool hasTrailingSemicolon;
	};

	struct MemberSpecifierNode
	{
		AstNode* memberDeclaration;
		AstNode* memberSpecification;
	};

	struct MemberAndAccessSpecifierNode
	{
		AstNode* accessSpecifier;
		AstNode* memberSpecification;
	};

	struct ClassKeyNode
	{
		Token token;
	};

	struct ClassVirtSpecifierNode
	{
		Token token;
	};

	struct ClassVirtSpecifierSeqNode
	{
		AstNode* thisSpec;
		AstNode* nextSpec;
	};

	struct ClassHeadNameNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* className;
	};

	struct ClassVirtualHeadNode
	{
		AstNode* classKey;
		AstNode* attributeSpecifierSeq;
		AstNode* classHeadName;
		AstNode* classVirtSpecifierSeq;
		AstNode* baseClause;
	};

	struct ClassHeadNode
	{
		AstNode* classKey;
		AstNode* attributeSpecifierSeq;
		AstNode* baseClause;
	};

	struct ClassSpecifierNode
	{
		AstNode* classHead;
		AstNode* memberSpecification;
	};

	struct ClassNameNode
	{
		Token identifier;
	};

	struct DeclaratorIdNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* className;
	};

	struct RefQualifierNode
	{
		bool doubleRef;
	};

	struct CvQualifierNode
	{
		Token qualifier;
	};

	struct CvQualifierSeqNode
	{
		AstNode* thisQualifier;
		AstNode* nextQualifier;
	};

	struct PtrStarNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* cvQualifierSeq;
	};

	struct RefNode
	{
		AstNode* attributeSpecifierSeq;
	};

	struct RefRefNode
	{
		AstNode* attributeSpecifierSeq;
	};

	struct PtrNamespaceStarNode
	{
		AstNode* nestedNameSpecifier;
		AstNode* attributeSpecifierSeq;
		AstNode* cvQualifierSeq;
	};

	struct TrailingReturnTypeNode
	{
		AstNode* trailingTypeSpecifierSeq;
		AstNode* abstractDeclarator;
	};

	struct ParametersAndQualifiersNode
	{
		AstNode* parameterDeclarationClause;
		AstNode* attributeSpecifierSeq;
		AstNode* cvQualifierSeq;
		AstNode* refQualifier;
		AstNode* exceptionSpecification;
	};

	struct PtrDeclaratorNode
	{
		AstNode* ptrOperator;
		AstNode* ptrDeclarator;
	};

	struct DeclaratorNode
	{
		AstNode* noPtrDeclarator;
		AstNode* parametersAndQualifiers;
		AstNode* trailingReturnType;
	};

	struct InitDeclaratorNode
	{
		AstNode* declarator;
		AstNode* initializer;
	};

	struct InitDeclaratorListNode
	{
		AstNode* thisDeclarator;
		AstNode* nextDeclarator;
	};

	struct BalancedTokenNode
	{
		Token token;
	};

	struct BalancedTokenSeqNode
	{
		AstNode* thisToken;
		AstNode* nextToken;
	};

	struct AttributeArgumentClauseNode
	{
		AstNode* balancedTokenSequence;
	};

	struct AttributeTokenNode
	{
		Token namespaceName;
		Token identifier;
	};

	struct AttributeNode
	{
		AstNode* attributeToken;
		AstNode* attributeArgumentClause;
	};

	struct AttributeListNode
	{
		AstNode* thisAttribute;
		AstNode* nextAttribute;
	};

	struct AlignAsTypeIdNode
	{
		AstNode* typeId;
		bool hasElipsis;
	};

	struct AlignAsExpressionNode
	{
		AstNode* alignmentExpression;
		bool hasElipsis;
	};

	struct AttributeSpecifierSequenceNode
	{
		AstNode* thisSpec;
		AstNode* nextSpec;
	};

	struct LinkageSpecificationBlockNode
	{
		Token stringLiteral;
		AstNode* declarationSeq;
	};

	struct LinkageSpecificationNode
	{
		Token stringLiteral;
		AstNode* declaration;
	};

	struct AsmNode
	{
		Token stringLiteral;
	};

	struct UsingDirectiveNode
	{
		AstNode* attributeSpecifierSeq;
		bool isNested;
		AstNode* nestedNameSpecifier;
		AstNode* namespaceName;
	};

	struct UsingDeclarationNode
	{
		AstNode* unqualifiedId;
	};

	struct UsingTypenameDeclarationNode
	{
		bool hasTypename;
		bool isNested;
		AstNode* nestedNameSpecifier;
		AstNode* unqualifiedId;
	};

	struct QualifiedNamespaceSpecifierNode
	{
		bool isNested;
		AstNode* nestedNameSpecifier;
		AstNode* namespaceName;
	};

	struct NamespaceAliasDefinitionNode
	{
		Token identifier;
		AstNode* qualifiedNamespaceSpecifier;
	};

	struct UnnamedNamespaceDefinitionNode
	{
		bool isInline;
		AstNode* namespaceBody;
	};

	struct NamedNamespaceDefinitionNode
	{
		bool isInline;
		Token identifier;
		AstNode* namespaceBody;
	};

	struct NamespaceNameNode
	{
		Token identifier;
	};

	struct ReturnNode
	{
		AstNode* returnValue;
	};

	struct GotoNode
	{
		Token identifier;
	};

	struct StatementSequenceNode
	{
		AstNode* thisStatement;
		AstNode* nextStatement;
	};

	struct CompoundStatementNode
	{
		AstNode* statementSequence;
	};

	struct LabeledIdentifierNode
	{
		AstNode* attributeSpecifierSeq;
		Token identifier;
		AstNode* statement;
	};

	struct CaseLabelNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* constantExpression;
		AstNode* statement;
	};

	struct DefaultLabelNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* statement;
	};

	struct QualifiedIdNode
	{
		Token identifier;
	};

	struct TemplateQualifiedIdNode
	{
		AstNode* nestedNamespaceSpecifier;
		bool hasNamespaceScope;
		bool hasTemplateKeyword;
	};

	struct StatementNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* statement;
	};

	struct ForRangeInitializerNode
	{
		AstNode* expression;
		AstNode* bracedInitList;
	};

	struct ForRangeDeclarationNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* typeSpecifierSeq;
		AstNode* declarator;
	};

	struct WhileLoopNode
	{
		AstNode* condition;
		AstNode* statement;
	};

	struct DoWhileLoopNode
	{
		AstNode* statement;
		AstNode* condition;
	};

	struct ForLoopNode
	{
		AstNode* forInitStatement;
		AstNode* condition;
		AstNode* expression;
		AstNode* statement;
	};

	struct ForEachLoopNode
	{
		AstNode* forRangeDeclaration;
		AstNode* forRangeInitializer;
		AstNode* statement;
	};

	struct InitializerConditionNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
		AstNode* initializerClause;
	};

	struct BracedInitConditionNode
	{
		AstNode* attributeSpecifierSeq;
		AstNode* declSpecifierSeq;
		AstNode* declarator;
		AstNode* bracedInitList;
	};

	struct IfElseNode
	{
		AstNode* condition;
		AstNode* ifStatement;
		AstNode* elseStatement;
	};

	struct SwitchNode
	{
		AstNode* condition;
		AstNode* statement;
	};

	struct ConstantExpressionNode
	{
		AstNode* expression;
	};

	struct EnumeratorDefinitionNode
	{
		Token identifier;
		AstNode* value;
	};

	struct EnumeratorListNode
	{
		AstNode* enumDefinition;
		AstNode* nextEnumDefinition;
	};

	struct EnumBaseNode
	{
		AstNode* TypeSpecifierSeq;
	};

	struct OpaqueEnumDeclNode
	{
		AstNode* enumKey;
		AstNode* attributeSpecifierSeq;
		Token identifier;
		AstNode* enumBase;
	};

	struct EnumHeadNode
	{
		AstNode* enumKey;
		AstNode* attributeSpecifierSeq;
		AstNode* nestedNameSpecifier;
		Token identifier;
		AstNode* enumBase;
	};

	struct EnumNameNode
	{
		Token identifier;
	};

	struct EnumSpecifierNode
	{
		AstNode* enumHead;
		AstNode* enumeratorList;
	};

	struct EnumKeyNode
	{
		EnumKeyType type;
	};

	struct TypeIdNode
	{
		AstNode* typeSpecifierSeq;
		AstNode* abstractDeclarator;
	};

	struct TemplateArgumentListNode
	{
		AstNode* thisArgument;
		AstNode* nextArgument;
	};

	struct LambdaExpressionNode
	{
		AstNode* introducer;
		AstNode* declarator;
		AstNode* compoundStatement;
	};

	struct LambdaIntroducerNode
	{
		AstNode* lambdaCapture;
	};

	struct LambdaCaptureNode
	{
		AstNode* captureList;
		bool hasDefaultRef;
		bool hasDefaultCopy;
	};

	struct CaptureNode
	{
		Token identifier;
	};

	struct LambdaCaptureListNode
	{
		AstNode* thisCapture;
		AstNode* nextCapture;
	};

	struct LambdaDeclaratorNode
	{
		AstNode* parameterDeclarationClause;
		AstNode* exceptionSpecification;
		AstNode* attributeSpecifierSequence;
		AstNode* trailingReturnType;
		bool isMutable;
	};

	struct GroupingNode
	{
		AstNode* expression;
	};

	struct LiteralNode
	{
		Token token;
	};

	struct ThisNode
	{
		Token token;
	};

	struct DeleteNode
	{
		AstNode* expression;
		bool deleteArray;
	};

	struct SizeofExpressionNode
	{
		AstNode* expression;
	};

	struct SizeofIdentifierExpressionNode
	{
		Token identifier;
	};

	struct AlignofExpressionNode
	{
		AstNode* expression;
	};

	struct UnaryExpressionNode
	{
		OverloadableOperatorType opType;
		AstNode* expression;
	};

	struct CastExpressionNode
	{
		AstNode* typeId;
		AstNode* expression;
	};

	struct PointerToMemberNode
	{
		AstNode* left;
		AstNode* right;
	};

	struct ExpressionNode
	{
		AstNode* expression;
		AstNode* nextExpression;
	};

	struct AssignmentExpressionNode
	{
		AstNode* leftSide;
		AssignmentOperatorType opType;
		AstNode* initializerClause;
	};

	struct TernaryExpressionNode
	{
		AstNode* comparisonExpression;
		AstNode* ifTrueNode;
		AstNode* ifFalseNode;
	};

	struct TypeParameterNode
	{
		AstNode* templateParameterList;
		AstNode* typeId;
		Token identifier;
	};

	struct BinaryExpressionNode
	{
		AstNode* left;
		AstNode* right;
		OverloadableOperatorType opType;
	};

	struct OperatorFunctionIdNode
	{
		OverloadableOperatorType opType;
		AstNode* templateArgList;
	};

	struct UsingAliasNode
	{
		AstNode* typeIdNode;
	};

	struct BracedInitListNode
	{
		AstNode* initializerList;
	};

	struct InitializerClauseNode
	{
		AstNode* clause;
	};

	struct AstNode
	{
		AstNode() {}
		~AstNode() {}

		AstNodeType type;
		bool success;
		union
		{
			UsingAliasNode usingAliasNode;
			BracedInitListNode bracedInitList;
			InitializerClauseNode initializerClauseNode;
			OperatorFunctionIdNode operatorFunctionId;
			TypeParameterNode typeParameter;
			BinaryExpressionNode binaryExpression;
			TernaryExpressionNode ternaryExpression;
			AssignmentExpressionNode assignmentExpression;
			ExpressionNode expressionNode;
			PointerToMemberNode pointerToMember;
			CastExpressionNode castExpression;
			UnaryExpressionNode unaryExpression;
			SizeofExpressionNode sizeofExpression;
			SizeofIdentifierExpressionNode sizeofIdentifierExpression;
			AlignofExpressionNode alignofExpression;
			DeleteNode deleteNode;
			LiteralNode literalNode;
			ThisNode thisNode;
			GroupingNode grouping;
			LambdaExpressionNode lambdaExpression;
			LambdaIntroducerNode lambdaIntroducer;
			LambdaCaptureNode lambdaCapture;
			CaptureNode capture;
			LambdaCaptureListNode lambdaCaptureList;
			LambdaDeclaratorNode lambdaDeclarator;
			TemplateArgumentListNode templateArgumentList;
			TemplateQualifiedIdNode templateQualifiedId;
			TypeIdNode typeIdNode;
			EnumNameNode enumName;
			EnumSpecifierNode enumSpecifier;
			EnumKeyNode enumKey;
			EnumHeadNode enumHead;
			OpaqueEnumDeclNode opaqueEnumDecl;
			EnumBaseNode enumBase;
			EnumeratorListNode enumeratorList;
			EnumeratorDefinitionNode enumeratorDefinition;
			ConstantExpressionNode constantExpression;
			IfElseNode ifElseNode;
			SwitchNode switchNode;
			InitializerConditionNode initCondition;
			BracedInitConditionNode bracedInitCondition;
			WhileLoopNode whileLoopNode;
			DoWhileLoopNode doWhileLoopNode;
			ForLoopNode forLoopNode;
			ForEachLoopNode forEachLoopNode;
			ForRangeDeclarationNode forRangeDeclaration;
			ForRangeInitializerNode forRangeInitializer;
			StatementNode statement;
			QualifiedIdNode qualifeidId;
			LabeledIdentifierNode labeledIdentifier;
			CaseLabelNode caseLabel;
			DefaultLabelNode defaultLabel;
			CompoundStatementNode compoundStatement;
			StatementSequenceNode statementSequence;
			ReturnNode returnNode;
			GotoNode gotoNode;
			NamespaceNameNode namespaceNameNode;
			NamedNamespaceDefinitionNode namedNamespaceDefinition;
			UnnamedNamespaceDefinitionNode unnamedNamespaceDefinition;
			NamespaceAliasDefinitionNode namespaceAliasDefinition;
			QualifiedNamespaceSpecifierNode qualifiedNamespaceSpecifier;
			UsingDeclarationNode usingDeclaration;
			UsingTypenameDeclarationNode usingTypenameDeclaration;
			UsingDirectiveNode usingDirective;
			AsmNode asmNode;
			LinkageSpecificationBlockNode linkageSpecificationBlock;
			LinkageSpecificationNode linkageSpecification;
			AttributeSpecifierSequenceNode attributeSpecifierSeq;
			AlignAsTypeIdNode alignAsTypeId;
			AlignAsExpressionNode alignAsExpression;
			AttributeListNode attributeList;
			AttributeNode attribute;
			AttributeTokenNode attributeToken;
			AttributeArgumentClauseNode attributeArgumentClause;
			BalancedTokenSeqNode balancedTokenSeq;
			BalancedTokenNode balancedToken;
			InitDeclaratorListNode initDeclaratorList;
			InitDeclaratorNode initDeclarator;
			DeclaratorNode declaratorNode;
			PtrDeclaratorNode ptrDeclarator;
			ParametersAndQualifiersNode parametersAndQualifiersNode;
			TrailingReturnTypeNode trailingReturnTypeNode;
			PtrStarNode ptrStar;
			RefNode ref;
			RefRefNode refRef;
			PtrNamespaceStarNode ptrNamespaceStar;
			CvQualifierSeqNode cvQualifierSeq;
			CvQualifierNode cvQualifier;
			RefQualifierNode refQualifier;
			DeclaratorIdNode declaratorId;
			ClassNameNode className;
			ClassSpecifierNode classSpecifier;
			ClassVirtualHeadNode classVirtualHead;
			ClassHeadNode classHead;
			ClassHeadNameNode classHeadName;
			ClassVirtSpecifierSeqNode classVirtSpecifierSeq;
			ClassVirtSpecifierNode classVirtSpecifier;
			ClassKeyNode classKey;
			MemberAndAccessSpecifierNode memberAndAccessSpecifier;
			MemberSpecifierNode memberSpecifier;
			MemberFunctionDeclarationNode memberFunctionDeclaration;
			MemberDeclarationNode memberDeclarationNode;
			MemberDeclaratorListNode memberDeclaratorList;
			MemberDeclaratorPureNode memberDeclaratorPure;
			MemberDeclaratorBraceNode memberDeclaratorBrace;
			MemberDeclaratorNode memberDeclarator;
			VirtSpecifierSeqNode virtSpecifierSeq;
			VirtSpecifierNode virtSpecifier;
			BaseSpecifierListNode baseSpecifierList;
			BaseSpecifierNode baseSpecifier;
			ClassOrDecltypeNode classOrDecltype;
			AccessSpecifierNode accessSpecifier;
			ConversionFunctionIdNode conversionFunctionId;
			ConversionTypeIdNode conversionTypeId;
			ConversionDeclaratorNode conversionDeclarator;
			MemInitializerListNode memInitializerList;
			MemExpressionInitializerNode memExpressionInitializer;
			MemBracedInitializerNode memBracedInitializer;
			MemInitializerIdNode memInitializerId;
			NestedNamespaceSpecifierIdNode nestedNamespaceSpecifierId;
			NestedNamespaceSpecifierTemplateNode nestedNamespaceSpecifierTemplate;
			PostfixSimpleTypeBraceListNode postfixSimpleTypeBraceList;
			PostfixSimpleTypeExpressionListNode postfixSimpleTypeExpressionList;
			PostfixTypenameSpecExpressionListNode postfixTypenameSpecExpressionList;
			PostfixTypenameSpecBraceListNode postfixTypenameSpecBraceList;
			PostfixCastNode postfixCast;
			PostfixTypeIdExpressionNode postfixTypeIdExpression;
			PostfixTypeIdNode postfixTypeId;
			PostfixBracketExpressionNode postfixBracketExpression;
			PostfixBracketBraceListNode postfixBracketBraceList;
			PostfixParenExpressionListNode postfixParenExpressionList;
			PostfixMemberIdExpressionNode postfixMemberIdExpression;
			PostfixPseudoDestructorNode postfixPseudoDestructor;
			PostfixPlusPlusNode postfixPlusPlus;
			PostfixMinusMinusNode postfixMinusMinus;
			PseudoDestructorDecltypeNode pseudoDestructorDecltype;
			PseudoDestructorTemplateNode pseudoDestructorTemplate;
			PseudoDestructorNode pseudoDestructor;
			PseudoNestedDestructorNode pseudoNestedDestructor;
			NewTypeIdExpressionNode newTypeIdExpression;
			NewExpressionNode newExpression;
			NewPlacementNode newPlacementNode;
			NewTypeIdNode newTypeId;
			NewDeclaratorNode newDeclarator;
			NoptrNewTailDeclaratorNode noptrNewTailDeclarator;
			NoptrNewDeclaratorNode noptrNewDeclarator;
			NewInitializerNode newInitializer;
			DeclarationSeqNode declarationSeq;
			AliasDeclarationNode aliasDeclaration;
			SimpleDeclarationNode simpleDeclaration;
			StaticAssertDeclarationNode staticAssertDeclaration;
			SimpleDeclSpecifierNode simpleDeclSpecifier;
			DeclSpecifierNode declSpecifier;
			DeclSpecSeqNode declSpecSeq;
			StorageClassSpecNode storageClassSpec;
			FunctionSpecNode functionSpec;
			TypedefNameNode typedefName;
			TypeSpecSeqNode typeSpecSeq;
			TrailingTypeSpecSeqNode trailingTypeSpecSeq;
			SimpleTypeTokenSpecNode simpleTypeTokenSpec;
			SimpleTypeTemplateSpecNode simpleTypeTemplateSpec;
			SimpleTypeSpecNode simpleTypeSpec;
			DecltypeSpecNode decltypeSpec;
			AbstractDeclaratorNode abstractDeclarator;
			PtrAbstractDeclaratorNode ptrAbstractDeclarator;
			ParameterDeclarationListNode parameterDeclarationList;
			ParameterDeclarationNode parameterDeclaration;
			ParameterDefaultDeclarationNode parameterDefaultDeclaration;
			ParameterAbstractDeclarationNode parameterAbstractDeclaration;
			ParameterAbstractDefaultDeclarationNode parameterAbstractDefaultDeclaration;
			FunctionDefaultDefinitionNode functionDefaultDefinition;
			FunctionDefinitionNode functionDefinition;
			FunctionBodyNode functionBody;
			LiteralOperatorIdNode literalOperatorId;
			TemplateDeclarationNode templateDeclaration;
			TemplateParameterListNode templateParameterList;
			SimpleTemplateIdNode simpleTemplateId;
			FunctionOperatorTemplateIdNode functionOperatorTemplateId;
			LiteralOperatorTemplateIdNode literalOperatorTemplateId;
			TemplateNameNode templateName;
			TypenameSpecifierNode typenameSpecifier;
			TypenameTemplateSpecifierNode typenameTemplateSpecifier;
			ExplicitInstantiationNode explicitInstantiation;
			TryBlockNode tryBlock;
			FunctionTryBlockNode functionTryBlock;
			HandlerSeqNode handlerSeq;
			HandlerNode handler;
			ExceptionDeclarationNode exceptionDeclaration;
			ExceptionAbstractDeclarationNode exceptionAbstractDeclaration;
			ThrowExpressionNode throwExpression;
			DynamicExceptionSpecNode dynamicExceptionSpec;
			TypeIdListNode typeIdList;
			NoexceptExpressionNode noexceptExpression;
			NoptrAbstractDeclaratorNode noptrAbstractDeclarator;
			NoptrAbstractExpressionDeclaratorNode noptrAbstractExpressionDeclarator;
			UnqualifiedIdNode unqualifiedId;
			UnqualifiedIdDtorClassNode unqualifiedIdDtorClass;
			UnqualifiedIdDtorDecltypeNode unqualifiedIdDtorDecltype;
			ElaboratedSpecifierEnumNode elaboratedSpecifierEnum;
			ElaboratedSpecifierTemplateNode elaboratedSpecifierTemplate;
			ElaboratedSpecifierClassNode elaboratedSpecifierClass;
			AlignmentExpressionNode alignmentExpression;
			NoPtrParenDeclaratorNode noPtrParenDeclarator;
			NoPtrBracketDeclaratorNode noPtrBracketDeclarator;
			NoPtrParamAndQualDeclaratorNode noPtrParamAndQualDeclarator;
			NoPtrDeclaratorNode noPtrDeclarator;
			USystemDeclarationNode uSystemDeclaration;
			InitializerListNode initializerList;
			TypeTemplateParameterNode typeTemplateParameter;
			TypeTypenameParameterNode typeTypenameParameter;
			TypeClassParameterNode typeClassParameter;
		};
	};

	struct HeaderNameStringNode
	{
		Token stringLiteral;
	};

	struct HeaderNameNode
	{
		Token identifier;
	};

	struct CharacterLiteralNode
	{
		Token characterLiteral;
	};

	struct StringLiteralNode
	{
		Token stringLiteral;
	};

	struct NumberLiteralNode
	{
		Token numberLiteral;
	};

	struct PPTokensNode
	{
		PreprocessingAstNode* preprocessingToken;
		PreprocessingAstNode* nextPreprocessingToken;
	};

	struct ReplacementListNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct IdentifierListNode
	{
		PreprocessingAstNode* thisIdentifierNode;
		PreprocessingAstNode* nextIdentifierNode;
	};

	struct IdentifierNode
	{
		Token identifier;
	};

	struct NonDirectiveNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct TextLineNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct MacroPragmaNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct MacroErrorNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct MacroLineNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct MacroUndefNode
	{
		Token identifier;
	};

	struct MacroDefineFunctionNode
	{
		Token identifier;
		PreprocessingAstNode* identifierList;
		PreprocessingAstNode* replacementList;
		bool endsInVariadicMacro;
	};

	struct MacroDefineNode
	{
		Token identifier;
		PreprocessingAstNode* replacementList;
	};

	struct MacroIncludeNode
	{
		PreprocessingAstNode* ppTokens;
	};

	struct ElseGroupNode
	{
		PreprocessingAstNode* group;
	};

	struct ElifGroupNode
	{
		AstNode* constantExpression;
		PreprocessingAstNode* group;
		bool evaluation;
	};

	struct ElifGroupsNode
	{
		PreprocessingAstNode* thisElifGroup;
		PreprocessingAstNode* nextElifGroup;
	};

	struct IfNDefGroupNode
	{
		Token identifier;
		PreprocessingAstNode* group;
		bool symbolDefined;
	};

	struct IfDefGroupNode
	{
		Token identifier;
		PreprocessingAstNode* group;
		bool symbolDefined;
	};

	struct IfGroupNode
	{
		AstNode* constantExpression;
		PreprocessingAstNode* group;
		bool evaluation;
	};

	struct IfSectionNode
	{
		PreprocessingAstNode* ifGroup;
		PreprocessingAstNode* elifGroups;
		PreprocessingAstNode* elseGroup;
	};

	struct GroupNode
	{
		PreprocessingAstNode* thisGroupPart;
		PreprocessingAstNode* nextGroupPart;
	};

	struct PreprocessingFileNode
	{
		PreprocessingAstNode* group;
	};

	struct PreprocessingOpOrPunc
	{
		Token opOrPunc;
	};

	struct WhitespaceNode
	{
		Token token;
	};

	struct PreprocessingAstNode
	{
		PreprocessingAstNode() {}
		~PreprocessingAstNode() {}

		PreprocessingAstNodeType type;
		bool success;

		union
		{
			// Preprocessing Stuff
			PreprocessingFileNode preprocessingFile;
			GroupNode group;
			IfSectionNode ifSection;
			IfGroupNode ifGroup;
			IfDefGroupNode ifDefGroup;
			IfNDefGroupNode ifNDefGroup;
			ElifGroupsNode elifGroups;
			ElifGroupNode elifGroup;
			ElseGroupNode elseGroup;
			MacroIncludeNode macroInclude;
			MacroDefineNode macroDefine;
			MacroDefineFunctionNode macroDefineFunction;
			MacroUndefNode macroUndef;
			MacroLineNode macroLine;
			MacroErrorNode macroError;
			MacroPragmaNode macroPragma;
			TextLineNode textLine;
			NonDirectiveNode nonDirective;
			IdentifierListNode identifierList;
			IdentifierNode identifier;
			ReplacementListNode replacementList;
			PPTokensNode ppTokens;
			StringLiteralNode stringLiteral;
			NumberLiteralNode numberLiteral;
			CharacterLiteralNode characterLiteral;
			HeaderNameNode headerName;
			HeaderNameStringNode headerNameString;
			PreprocessingOpOrPunc preprocessingOpOrPunc;
			WhitespaceNode whitespace;
		};
	};
}

#endif