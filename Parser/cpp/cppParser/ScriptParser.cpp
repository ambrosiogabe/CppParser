#include "cppParser/ScriptParser.h"
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"
#include "cppParser/ScriptScanner.h"
#include "cppParser/Symbols.h"
#include "CppUtils/CppUtils.h"

#include <algorithm>
#include <cstring>

namespace CppParser
{
	namespace Parser
	{
		// Using statements
		using namespace CppUtils;

		// Internal variables
		static PPSymbolTable PreprocessingSymbolTable;
		static std::vector<std::filesystem::path> FilesSeen = {};

		// This is not a POD because of the list
		struct ParserData
		{
			List<Token> Tokens;
			int CurrentToken;
		};

		static AstNode* ParseTranslationUnit(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, ParserData& data);

		AstNode* Parse(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, List<Token>& tokens)
		{
			FilesSeen.clear();
			ParserData data = {
				tokens,
				0
			};

			AstNode* result = ParseTranslationUnit(fileBeingParsed, includeDirs, data);
			return result;
		}

		// =============================================================================================
		// Free Node functions (Internal use only)
		//
		// These will recursively walk whatever node is passed in and free the memory allocated
		// by the node itself. These functions will *NOT* free any memory that is contained
		// within the tokens. So strings that were allocated have to be freed by the scanner.
		// =============================================================================================
		static void FreeNodeCallback(AstNode* tree)
		{
			FreeMem(tree);
		}

		static void FreeNode(AstNode* node)
		{
			WalkTree(node, FreeNodeCallback, AstNodeType::All, true);
		}

		static void FreePreprocessingNodeCallback(PreprocessingAstNode* tree)
		{
			FreeMem(tree);
		}

		static void FreePreprocessingNode(PreprocessingAstNode* node)
		{
			WalkPreprocessingTree(node, FreePreprocessingNodeCallback, PreprocessingAstNodeType::All, true);
		}

		// =============================================================================================
		// Free Tree function (public)
		//
		// This will free all memory allocated by the AST tree passed in
		// =============================================================================================
		void FreeTree(AstNode* tree)
		{
			FreeNode(tree);
		}

		// ===============================================================================================
		// Walk Tree functions
		//
		// These will recursively walk preprocessing, or regular ast trees and call the callback function
		// indicated by the notification type
		// ===============================================================================================
		// TODO: Is there a better way to do this function callback with default parameter?
		static void WalkTreeDefaultUserData(AstNode* tree, void* userData)
		{
			((AstWalkTreeCallbackFn)(userData))(tree);
		}

		void WalkTree(AstNode* tree, AstWalkTreeCallbackFn callbackFn, AstNodeType notificationType, bool postTraversalCallback)
		{
			WalkTree(tree, callbackFn, WalkTreeDefaultUserData, notificationType, postTraversalCallback);
		}

		static void WalkPpTreeDefaultUserData(PreprocessingAstNode* tree, void* userData)
		{
			((AstWalkPpTreeCallbackFn)(userData))(tree);
		}

		void WalkPreprocessingTree(PreprocessingAstNode* tree, AstWalkPpTreeCallbackFn callbackFn, PreprocessingAstNodeType notificationType, bool postTraversalCallback)
		{
			WalkPreprocessingTree(tree, callbackFn, WalkPpTreeDefaultUserData, notificationType, postTraversalCallback);
		}

		void WalkTree(AstNode* tree, void* userData, AstWalkTreeUserDataCallbackFn callbackFn, AstNodeType notificationType, bool postTraversalCallback)
		{
			if (!postTraversalCallback && (notificationType == AstNodeType::All || tree->type == notificationType))
			{
				callbackFn(tree, userData);
			}

#define WALK(node) WalkTree(node, userData, callbackFn, notificationType, postTraversalCallback)
			switch (tree->type)
			{
			case AstNodeType::BracedInitList:
				WALK(tree->bracedInitList.initializerList);
				break;
			case AstNodeType::InitializerClause:
				WALK(tree->initializerClauseNode.clause);
				break;
			case AstNodeType::InitializerList:
				WALK(tree->initializerList.thisInitList);
				WALK(tree->initializerList.nextInitList);
				break;
			case AstNodeType::OperatorFunctionId:
				WALK(tree->operatorFunctionId.templateArgList);
				break;
			case AstNodeType::BinaryExpression:
				WALK(tree->binaryExpression.left);
				WALK(tree->binaryExpression.right);
				break;
			case AstNodeType::TernaryExpression:
				WALK(tree->ternaryExpression.comparisonExpression);
				WALK(tree->ternaryExpression.ifTrueNode);
				WALK(tree->ternaryExpression.ifFalseNode);
				break;
			case AstNodeType::AssignmentExpression:
				WALK(tree->assignmentExpression.leftSide);
				WALK(tree->assignmentExpression.initializerClause);
				break;
			case AstNodeType::Expression:
				WALK(tree->expressionNode.expression);
				WALK(tree->expressionNode.nextExpression);
				break;
			case AstNodeType::PointerToMember:
				WALK(tree->pointerToMember.left);
				WALK(tree->pointerToMember.right);
				break;
			case AstNodeType::CastExpression:
				WALK(tree->castExpression.expression);
				WALK(tree->castExpression.typeId);
				break;
			case AstNodeType::UnaryExpression:
				WALK(tree->unaryExpression.expression);
				break;
			case AstNodeType::SizeofExpression:
				WALK(tree->sizeofExpression.expression);
				break;
			case AstNodeType::SizeofIdentifierExpression:
				break;
			case AstNodeType::AlignofExpression:
				WALK(tree->alignofExpression.expression);
				break;
			case AstNodeType::Delete:
				WALK(tree->deleteNode.expression);
				break;
			case AstNodeType::Literal:
				break;
			case AstNodeType::This:
				break;
			case AstNodeType::Grouping:
				WALK(tree->grouping.expression);
				break;
			case AstNodeType::LambdaExpression:
				WALK(tree->lambdaExpression.compoundStatement);
				WALK(tree->lambdaExpression.declarator);
				WALK(tree->lambdaExpression.introducer);
				break;
			case AstNodeType::LambdaIntroducer:
				WALK(tree->lambdaIntroducer.lambdaCapture);
				break;
			case AstNodeType::LambdaCapture:
				WALK(tree->lambdaCapture.captureList);
				break;
			case AstNodeType::Capture:
				break;
			case AstNodeType::LambdaCaptureList:
				WALK(tree->lambdaCaptureList.thisCapture);
				WALK(tree->lambdaCaptureList.nextCapture);
				break;
			case AstNodeType::LambdaDeclarator:
				WALK(tree->lambdaDeclarator.attributeSpecifierSequence);
				WALK(tree->lambdaDeclarator.exceptionSpecification);
				WALK(tree->lambdaDeclarator.parameterDeclarationClause);
				WALK(tree->lambdaDeclarator.trailingReturnType);
				break;
			case AstNodeType::TemplateArgumentList:
				WALK(tree->templateArgumentList.thisArgument);
				WALK(tree->templateArgumentList.nextArgument);
				break;
			case AstNodeType::TemplateQualifiedId:
				WALK(tree->templateQualifiedId.nestedNamespaceSpecifier);
				break;
			case AstNodeType::TypeId:
				WALK(tree->typeIdNode.abstractDeclarator);
				WALK(tree->typeIdNode.typeSpecifierSeq);
			case AstNodeType::EnumName:
				break;
			case AstNodeType::EnumSpecifier:
				WALK(tree->enumSpecifier.enumHead);
				WALK(tree->enumSpecifier.enumeratorList);
				break;
			case AstNodeType::EnumKey:
				break;
			case AstNodeType::EnumHead:
				WALK(tree->enumHead.attributeSpecifierSeq);
				WALK(tree->enumHead.enumBase);
				WALK(tree->enumHead.enumKey);
				WALK(tree->enumHead.nestedNameSpecifier);
				break;
			case AstNodeType::OpaqueEnumDecl:
				WALK(tree->opaqueEnumDecl.attributeSpecifierSeq);
				WALK(tree->opaqueEnumDecl.enumBase);
				WALK(tree->opaqueEnumDecl.enumKey);
				break;
			case AstNodeType::EnumBase:
				WALK(tree->enumBase.TypeSpecifierSeq);
				break;
			case AstNodeType::EnumeratorList:
				WALK(tree->enumeratorList.enumDefinition);
				WALK(tree->enumeratorList.nextEnumDefinition);
				break;
			case AstNodeType::EnumeratorDefinition:
				WALK(tree->enumeratorDefinition.value);
				break;
			case AstNodeType::ConstantExpression:
				WALK(tree->constantExpression.expression);
				break;
			case AstNodeType::IfElse:
				WALK(tree->ifElseNode.condition);
				WALK(tree->ifElseNode.ifStatement);
				WALK(tree->ifElseNode.elseStatement);
				break;
			case AstNodeType::Switch:
				WALK(tree->switchNode.condition);
				WALK(tree->switchNode.statement);
				break;
			case AstNodeType::InitializerCondition:
				WALK(tree->initCondition.attributeSpecifierSeq);
				WALK(tree->initCondition.declarator);
				WALK(tree->initCondition.declSpecifierSeq);
				WALK(tree->initCondition.initializerClause);
				break;
			case AstNodeType::BracedInitCondition:
				WALK(tree->bracedInitCondition.attributeSpecifierSeq);
				WALK(tree->bracedInitCondition.bracedInitList);
				WALK(tree->bracedInitCondition.declarator);
				WALK(tree->bracedInitCondition.declSpecifierSeq);
				break;
			case AstNodeType::WhileLoop:
				WALK(tree->whileLoopNode.condition);
				WALK(tree->whileLoopNode.statement);
				break;
			case AstNodeType::DoWhileLoop:
				WALK(tree->doWhileLoopNode.condition);
				WALK(tree->doWhileLoopNode.statement);
				break;
			case AstNodeType::ForLoop:
				WALK(tree->forLoopNode.condition);
				WALK(tree->forLoopNode.expression);
				WALK(tree->forLoopNode.forInitStatement);
				WALK(tree->forLoopNode.statement);
				break;
			case AstNodeType::ForEachLoop:
				WALK(tree->forEachLoopNode.forRangeDeclaration);
				WALK(tree->forEachLoopNode.forRangeInitializer);
				WALK(tree->forEachLoopNode.statement);
				break;
			case AstNodeType::ForRangeDeclaration:
				WALK(tree->forRangeDeclaration.attributeSpecifierSeq);
				WALK(tree->forRangeDeclaration.declarator);
				WALK(tree->forRangeDeclaration.typeSpecifierSeq);
				break;
			case AstNodeType::ForRangeInitializer:
				WALK(tree->forRangeInitializer.bracedInitList);
				WALK(tree->forRangeInitializer.expression);
				break;
			case AstNodeType::Statement:
				WALK(tree->statement.attributeSpecifierSeq);
				WALK(tree->statement.statement);
				break;
			case AstNodeType::QualifiedId:
				break;
			case AstNodeType::LabeledIdentifier:
				WALK(tree->labeledIdentifier.attributeSpecifierSeq);
				WALK(tree->labeledIdentifier.statement);
				break;
			case AstNodeType::CaseLabel:
				WALK(tree->caseLabel.attributeSpecifierSeq);
				WALK(tree->caseLabel.constantExpression);
				WALK(tree->caseLabel.statement);
				break;
			case AstNodeType::DefaultLabel:
				WALK(tree->defaultLabel.attributeSpecifierSeq);
				WALK(tree->defaultLabel.statement);
				break;
			case AstNodeType::CompoundStatement:
				WALK(tree->compoundStatement.statementSequence);
				break;
			case AstNodeType::StatementSequence:
				WALK(tree->statementSequence.thisStatement);
				WALK(tree->statementSequence.nextStatement);
				break;
			case AstNodeType::Break:
				break;
			case AstNodeType::Continue:
				break;
			case AstNodeType::Goto:
				break;
			case AstNodeType::Return:
				WALK(tree->returnNode.returnValue);
				break;
			case AstNodeType::NamespaceName:
				break;
			case AstNodeType::NamedNamespaceDefinition:
				WALK(tree->namedNamespaceDefinition.namespaceBody);
				break;
			case AstNodeType::UnnamedNamespaceDefinition:
				WALK(tree->unnamedNamespaceDefinition.namespaceBody);
				break;
			case AstNodeType::NamespaceAliasDefinition:
				WALK(tree->namespaceAliasDefinition.qualifiedNamespaceSpecifier);
				break;
			case AstNodeType::QualifiedNamespaceSpecifier:
				WALK(tree->qualifiedNamespaceSpecifier.namespaceName);
				WALK(tree->qualifiedNamespaceSpecifier.nestedNameSpecifier);
				break;
			case AstNodeType::UsingDeclaration:
				WALK(tree->usingDeclaration.unqualifiedId);
				break;
			case AstNodeType::UsingTypenameDeclaration:
				WALK(tree->usingTypenameDeclaration.nestedNameSpecifier);
				WALK(tree->usingTypenameDeclaration.unqualifiedId);
				break;
			case AstNodeType::UsingDirective:
				WALK(tree->usingDirective.attributeSpecifierSeq);
				WALK(tree->usingDirective.namespaceName);
				WALK(tree->usingDirective.nestedNameSpecifier);
				break;
			case AstNodeType::Asm:
				break;
			case AstNodeType::LinkageSpecificationBlock:
				WALK(tree->linkageSpecificationBlock.declarationSeq);
				break;
			case AstNodeType::LinkageSpecification:
				WALK(tree->linkageSpecification.declaration);
				break;
			case AstNodeType::AttributeSpecifierSequence:
				WALK(tree->attributeSpecifierSeq.thisSpec);
				WALK(tree->attributeSpecifierSeq.nextSpec);
				break;
			case AstNodeType::AlignAsTypeId:
				WALK(tree->alignAsTypeId.typeId);
				break;
			case AstNodeType::AlignAsExpression:
				WALK(tree->alignAsExpression.alignmentExpression);
				break;
			case AstNodeType::AttributeList:
				WALK(tree->attributeList.thisAttribute);
				WALK(tree->attributeList.nextAttribute);
				break;
			case AstNodeType::EmptyAttributeList:
				break;
			case AstNodeType::Attribute:
				WALK(tree->attribute.attributeArgumentClause);
				WALK(tree->attribute.attributeToken);
				break;
			case AstNodeType::AttributeToken:
				break;
			case AstNodeType::AttributeArgumentClause:
				WALK(tree->attributeArgumentClause.balancedTokenSequence);
				break;
			case AstNodeType::BalancedTokenSeq:
				WALK(tree->balancedTokenSeq.thisToken);
				WALK(tree->balancedTokenSeq.nextToken);
				break;
			case AstNodeType::BalancedToken:
				break;
			case AstNodeType::InitDeclaratorList:
				WALK(tree->initDeclaratorList.thisDeclarator);
				WALK(tree->initDeclaratorList.nextDeclarator);
				break;
			case AstNodeType::InitDeclarator:
				WALK(tree->initDeclarator.declarator);
				WALK(tree->initDeclarator.initializer);
				break;
			case AstNodeType::Declarator:
				WALK(tree->declaratorNode.noPtrDeclarator);
				WALK(tree->declaratorNode.parametersAndQualifiers);
				WALK(tree->declaratorNode.trailingReturnType);
				break;
			case AstNodeType::PtrDeclarator:
				WALK(tree->ptrDeclarator.ptrDeclarator);
				WALK(tree->ptrDeclarator.ptrOperator);
				break;
			case AstNodeType::ParametersAndQualifiers:
				WALK(tree->parametersAndQualifiersNode.attributeSpecifierSeq);
				WALK(tree->parametersAndQualifiersNode.cvQualifierSeq);
				WALK(tree->parametersAndQualifiersNode.exceptionSpecification);
				WALK(tree->parametersAndQualifiersNode.parameterDeclarationClause);
				WALK(tree->parametersAndQualifiersNode.refQualifier);
				break;
			case AstNodeType::TrailingReturnType:
				WALK(tree->trailingReturnTypeNode.abstractDeclarator);
				WALK(tree->trailingReturnTypeNode.trailingTypeSpecifierSeq);
				break;
			case AstNodeType::PtrStar:
				WALK(tree->ptrStar.attributeSpecifierSeq);
				WALK(tree->ptrStar.cvQualifierSeq);
				break;
			case AstNodeType::Ref:
				WALK(tree->ref.attributeSpecifierSeq);
				break;
			case AstNodeType::RefRef:
				WALK(tree->refRef.attributeSpecifierSeq);
				break;
			case AstNodeType::PtrNamespaceStar:
				WALK(tree->ptrNamespaceStar.attributeSpecifierSeq);
				WALK(tree->ptrNamespaceStar.cvQualifierSeq);
				WALK(tree->ptrNamespaceStar.nestedNameSpecifier);
				break;
			case AstNodeType::CvQualifierSeq:
				WALK(tree->cvQualifierSeq.thisQualifier);
				WALK(tree->cvQualifierSeq.nextQualifier);
				break;
			case AstNodeType::CvQualifier:
				break;
			case AstNodeType::RefQualifier:
				break;
			case AstNodeType::DeclaratorId:
				WALK(tree->declaratorId.className);
				WALK(tree->declaratorId.nestedNameSpecifier);
				break;
			case AstNodeType::ClassName:
				break;
			case AstNodeType::ClassSpecifier:
				WALK(tree->classSpecifier.classHead);
				WALK(tree->classSpecifier.memberSpecification);
				break;
			case AstNodeType::ClassHead:
				WALK(tree->classHead.attributeSpecifierSeq);
				WALK(tree->classHead.baseClause);
				WALK(tree->classHead.classKey);
				break;
			case AstNodeType::ClassVirtualHead:
				WALK(tree->classVirtualHead.attributeSpecifierSeq);
				WALK(tree->classVirtualHead.baseClause);
				WALK(tree->classVirtualHead.classHeadName);
				WALK(tree->classVirtualHead.classKey);
				WALK(tree->classVirtualHead.classVirtSpecifierSeq);
				break;
			case AstNodeType::ClassHeadName:
				WALK(tree->classHeadName.className);
				WALK(tree->classHeadName.nestedNameSpecifier);
				break;
			case AstNodeType::ClassVirtSpecifierSeq:
				WALK(tree->classVirtSpecifierSeq.thisSpec);
				WALK(tree->classVirtSpecifierSeq.nextSpec);
				break;
			case AstNodeType::ClassVirtSpecifier:
				break;
			case AstNodeType::ClassKey:
				break;
			case AstNodeType::MemberAndAccessSpecifier:
				WALK(tree->memberAndAccessSpecifier.accessSpecifier);
				WALK(tree->memberAndAccessSpecifier.memberSpecification);
				break;
			case AstNodeType::MemberSpecifier:
				WALK(tree->memberSpecifier.memberDeclaration);
				WALK(tree->memberSpecifier.memberSpecification);
				break;
			case AstNodeType::MemberFunctionDeclaration:
				WALK(tree->memberFunctionDeclaration.functionDefinition);
				break;
			case AstNodeType::MemberDeclaration:
				WALK(tree->memberDeclarationNode.attribtueSpecifierSeq);
				WALK(tree->memberDeclarationNode.declSpecifierSeq);
				WALK(tree->memberDeclarationNode.memberDeclaratorList);
				break;
			case AstNodeType::MemberDeclaratorList:
				WALK(tree->memberDeclaratorList.thisDeclarator);
				WALK(tree->memberDeclaratorList.nextDeclarator);
				break;
			case AstNodeType::MemberDeclaratorPure:
				WALK(tree->memberDeclaratorPure.declarator);
				WALK(tree->memberDeclaratorPure.pureSpecifier);
				WALK(tree->memberDeclaratorPure.virtSpecifierSeq);
				break;
			case AstNodeType::MemberDeclaratorBrace:
				WALK(tree->memberDeclaratorBrace.braceOrEqualInitializer);
				WALK(tree->memberDeclaratorBrace.declarator);
				WALK(tree->memberDeclaratorBrace.virtSpecifierSeq);
				break;
			case AstNodeType::MemberDeclarator:
				WALK(tree->memberDeclarator.attributeSpecifierSeq);
				WALK(tree->memberDeclarator.constantExpression);
				WALK(tree->memberDeclarator.virtSpecifierSeq);
				break;
			case AstNodeType::VirtSpecifierSeq:
				WALK(tree->virtSpecifierSeq.thisSpec);
				WALK(tree->virtSpecifierSeq.nextSpec);
				break;
			case AstNodeType::VirtSpecifier:
				break;
			case AstNodeType::PureSpecifier:
				break;
			case AstNodeType::BaseSpecifierList:
				WALK(tree->baseSpecifierList.thisBaseSpecifier);
				WALK(tree->baseSpecifierList.nextBaseSpecifier);
				break;
			case AstNodeType::BaseSpecifier:
				WALK(tree->baseSpecifier.accessSpecifier);
				WALK(tree->baseSpecifier.attributeSpecifierSeq);
				WALK(tree->baseSpecifier.baseTypeSpecifier);
				break;
			case AstNodeType::ClassOrDecltype:
				WALK(tree->classOrDecltype.className);
				WALK(tree->classOrDecltype.nestedNameSpecifier);
				break;
			case AstNodeType::AccessSpecifier:
				break;
			case AstNodeType::ConversionFunctionId:
				WALK(tree->conversionFunctionId.conversionTypeId);
				break;
			case AstNodeType::ConversionTypeId:
				WALK(tree->conversionTypeId.conversionDeclarator);
				WALK(tree->conversionTypeId.typeSpecifierSeq);
				break;
			case AstNodeType::ConversionDeclarator:
				WALK(tree->conversionDeclarator.conversionDeclarator);
				WALK(tree->conversionDeclarator.ptrOperator);
				break;
			case AstNodeType::MemInitializerList:
				WALK(tree->memInitializerList.thisMemInitializer);
				WALK(tree->memInitializerList.nextMemInitializer);
				break;
			case AstNodeType::MemExpressionInitializer:
				WALK(tree->memExpressionInitializer.expressionList);
				WALK(tree->memExpressionInitializer.memInitializerId);
				break;
			case AstNodeType::MemBracedInitializer:
				WALK(tree->memBracedInitializer.bracedInitList);
				WALK(tree->memBracedInitializer.memInitializerId);
				break;
			case AstNodeType::MemInitializerId:
				break;
			case AstNodeType::NestedNamespaceSpecifierId:
				WALK(tree->nestedNamespaceSpecifierId.nestedNameSpecifier);
				break;
			case AstNodeType::NestedNamespaceSpecifierTemplate:
				WALK(tree->nestedNamespaceSpecifierTemplate.nestedNameSpecifier);
				WALK(tree->nestedNamespaceSpecifierTemplate.simpleTemplateId);
				break;
			case AstNodeType::PostfixSimpleTypeExpressionList:
				WALK(tree->postfixSimpleTypeExpressionList.expressionList);
				WALK(tree->postfixSimpleTypeExpressionList.simpleTypeSpecifier);
				break;
			case AstNodeType::PostfixSimpleTypeBraceList:
				WALK(tree->postfixSimpleTypeBraceList.bracedInitList);
				WALK(tree->postfixSimpleTypeBraceList.simpleTypeSpecifier);
				break;
			case AstNodeType::PostfixTypenameSpecExpressionList:
				WALK(tree->postfixSimpleTypeBraceList.bracedInitList);
				WALK(tree->postfixSimpleTypeBraceList.simpleTypeSpecifier);
				break;
			case AstNodeType::PostfixTypenameSpecBraceList:
				WALK(tree->postfixTypenameSpecBraceList.bracedInitList);
				WALK(tree->postfixTypenameSpecBraceList.typenameSpecifier);
				break;
			case AstNodeType::PostfixCast:
				WALK(tree->postfixCast.expression);
				WALK(tree->postfixCast.typeId);
				break;
			case AstNodeType::PostfixTypeIdExpression:
				WALK(tree->postfixTypeIdExpression.expression);
				break;
			case AstNodeType::PostfixTypeId:
				WALK(tree->postfixTypeId.typeId);
				break;
			case AstNodeType::PostfixBracketExpression:
				WALK(tree->postfixBracketExpression.expression);
				WALK(tree->postfixBracketExpression.postfixExpression);
				break;
			case AstNodeType::PostfixBracketBraceList:
				WALK(tree->postfixBracketBraceList.bracedInitList);
				WALK(tree->postfixBracketBraceList.postfixExpression);
				break;
			case AstNodeType::PostfixParenExpressionList:
				WALK(tree->postfixParenExpressionList.expressionList);
				WALK(tree->postfixParenExpressionList.postfixExpression);
				break;
			case AstNodeType::PostfixMemberIdExpression:
				WALK(tree->postfixMemberIdExpression.idExpression);
				WALK(tree->postfixMemberIdExpression.postfixExpression);
				break;
			case AstNodeType::PostfixPseudoDestructor:
				WALK(tree->postfixPseudoDestructor.postfixExpression);
				WALK(tree->postfixPseudoDestructor.pseudoDestructorName);
				break;
			case AstNodeType::PostfixPlusPlus:
				WALK(tree->postfixPlusPlus.postfixExpression);
				break;
			case AstNodeType::PostfixMinusMinus:
				WALK(tree->postfixMinusMinus.postfixExpression);
				break;
			case AstNodeType::PseudoDestructorDecltype:
				WALK(tree->pseudoDestructorDecltype.decltypeSpecifier);
				break;
			case AstNodeType::PseudoDestructorTemplate:
				WALK(tree->pseudoDestructorTemplate.nestedNameSpecifier);
				WALK(tree->pseudoDestructorTemplate.simpleTemplateId);
				WALK(tree->pseudoDestructorTemplate.typeName);
				break;
			case AstNodeType::PseudoDestructor:
				WALK(tree->pseudoDestructor.nestedNameSpecifier);
				WALK(tree->pseudoDestructor.typeName);
				break;
			case AstNodeType::PseudoNestedDestructor:
				WALK(tree->pseudoNestedDestructor.nestedNameSpecifier);
				WALK(tree->pseudoNestedDestructor.nestedTypeName);
				WALK(tree->pseudoNestedDestructor.typeName);
				break;
			case AstNodeType::NewTypeIdExpression:
				WALK(tree->newTypeIdExpression.newInitializer);
				WALK(tree->newTypeIdExpression.newPlacement);
				WALK(tree->newTypeIdExpression.newTypeId);
				break;
			case AstNodeType::NewExpression:
				WALK(tree->newExpression.newInitializer);
				WALK(tree->newExpression.newPlacement);
				WALK(tree->newExpression.typeId);
				break;
			case AstNodeType::NewPlacement:
				WALK(tree->newPlacementNode.expressionList);
				break;
			case AstNodeType::NewTypeId:
				WALK(tree->newTypeId.newDeclarator);
				WALK(tree->newTypeId.typeSpecifierSeq);
				break;
			case AstNodeType::NewDeclarator:
				WALK(tree->newDeclarator.newDeclarator);
				WALK(tree->newDeclarator.ptrOperator);
				break;
			case AstNodeType::NoptrNewTailDeclarator:
				WALK(tree->noptrNewTailDeclarator.attributeSpecifierSeq);
				WALK(tree->noptrNewTailDeclarator.expression);
				break;
			case AstNodeType::NoptrNewDeclarator:
				WALK(tree->noptrNewDeclarator.attributeSpecifierSeq);
				WALK(tree->noptrNewDeclarator.constantExpression);
				WALK(tree->noptrNewDeclarator.noptrNewDeclarator);
				break;
			case AstNodeType::NewInitializer:
				WALK(tree->newInitializer.expressionList);
				break;
			case AstNodeType::DeclarationSeq:
				WALK(tree->declarationSeq.thisDeclaration);
				WALK(tree->declarationSeq.nextDeclaration);
				break;
			case AstNodeType::AliasDeclaration:
				WALK(tree->aliasDeclaration.typeId);
				break;
			case AstNodeType::SimpleDeclaration:
				WALK(tree->simpleDeclaration.attributeSpecifierSeq);
				WALK(tree->simpleDeclaration.declSpecifierSeq);
				WALK(tree->simpleDeclaration.initDeclaratorList);
				break;
			case AstNodeType::StaticAssertDeclaration:
				WALK(tree->staticAssertDeclaration.constantExpression);
				break;
			case AstNodeType::SimpleDeclSpecifier:
				break;
			case AstNodeType::DeclSpecifier:
				WALK(tree->declSpecifier.specifier);
				break;
			case AstNodeType::DeclSpecSeq:
				WALK(tree->declSpecSeq.attributeSpecifierSeq);
				WALK(tree->declSpecSeq.thisSpec);
				WALK(tree->declSpecSeq.nextSpec);
				break;
			case AstNodeType::StorageClassSpec:
				break;
			case AstNodeType::FunctionSpec:
				break;
			case AstNodeType::TypedefName:
				break;
			case AstNodeType::TypeSpecSeq:
				WALK(tree->typeSpecSeq.attributeSpecifierSeq);
				WALK(tree->typeSpecSeq.thisTypeSpec);
				WALK(tree->typeSpecSeq.nextTypeSpec);
				break;
			case AstNodeType::TrailingTypeSpecSeq:
				WALK(tree->trailingTypeSpecSeq.attributeSpecifierSeq);
				WALK(tree->trailingTypeSpecSeq.thisTypeSpec);
				WALK(tree->trailingTypeSpecSeq.nextTypeSpec);
				break;
			case AstNodeType::SimpleTypeTokenSpec:
				break;
			case AstNodeType::SimpleTypeTemplateSpec:
				WALK(tree->simpleTypeTemplateSpec.nestedNameSpec);
				WALK(tree->simpleTypeTemplateSpec.simpleTemplateId);
				break;
			case AstNodeType::SimpleTypeSpec:
				WALK(tree->simpleTypeSpec.nestedNameSpec);
				WALK(tree->simpleTypeSpec.typeName);
				break;
			case AstNodeType::DecltypeSpec:
				WALK(tree->decltypeSpec.expression);
				break;
			case AstNodeType::AbstractDeclarator:
				WALK(tree->abstractDeclarator.noptrAbstractDeclarator);
				WALK(tree->abstractDeclarator.parametersAndQualifiers);
				WALK(tree->abstractDeclarator.trailingReturnType);
				break;
			case AstNodeType::AbstractElipsisDeclarator:
				break;
			case AstNodeType::PtrAbstractDeclarator:
				WALK(tree->ptrAbstractDeclarator.ptrAbstractDeclarator);
				WALK(tree->ptrAbstractDeclarator.ptrOperator);
				break;
			case AstNodeType::ParameterDeclarationList:
				WALK(tree->parameterDeclarationList.thisParameter);
				WALK(tree->parameterDeclarationList.nextParameter);
				break;
			case AstNodeType::ParameterDefaultDeclaration:
				WALK(tree->parameterDefaultDeclaration.attributeSpecifierSeq);
				WALK(tree->parameterDefaultDeclaration.declarator);
				WALK(tree->parameterDefaultDeclaration.declSpecifierSeq);
				WALK(tree->parameterDefaultDeclaration.initializerClause);
				break;
			case AstNodeType::ParameterDeclaration:
				WALK(tree->parameterDeclaration.attributeSpecifierSeq);
				WALK(tree->parameterDeclaration.declarator);
				WALK(tree->parameterDeclaration.declSpecifierSeq);
				break;
			case AstNodeType::ParameterAbstractDefaultDeclaration:
				WALK(tree->parameterAbstractDefaultDeclaration.abstractDeclarator);
				WALK(tree->parameterAbstractDefaultDeclaration.attributeSpecifierSeq);
				WALK(tree->parameterAbstractDefaultDeclaration.declSpecifierSeq);
				WALK(tree->parameterAbstractDefaultDeclaration.initializerClause);
				break;
			case AstNodeType::ParameterAbstractDeclaration:
				WALK(tree->parameterAbstractDeclaration.abstractDeclarator);
				WALK(tree->parameterAbstractDeclaration.attributeSpecifierSeq);
				WALK(tree->parameterAbstractDeclaration.declSpecifierSeq);
				break;
			case AstNodeType::FunctionDefaultDefinition:
				WALK(tree->functionDefaultDefinition.attributeSpecifierSeq);
				WALK(tree->functionDefaultDefinition.declarator);
				WALK(tree->functionDefaultDefinition.declSpecifierSeq);
				break;
			case AstNodeType::FunctionDefinition:
				WALK(tree->functionDefinition.attributeSpecifierSeq);
				WALK(tree->functionDefinition.declarator);
				WALK(tree->functionDefinition.declSpecifierSeq);
				WALK(tree->functionDefinition.functionBody);
				break;
			case AstNodeType::FunctionBody:
				WALK(tree->functionBody.compoundStatement);
				WALK(tree->functionBody.ctorInitializer);
				break;
			case AstNodeType::LiteralOperatorId:
				break;
			case AstNodeType::TemplateDeclaration:
				WALK(tree->templateDeclaration.declaration);
				WALK(tree->templateDeclaration.templateParameterList);
				break;
			case AstNodeType::TemplateParameterList:
				WALK(tree->templateParameterList.thisParameter);
				WALK(tree->templateParameterList.nextParameter);
				break;
			case AstNodeType::SimpleTemplateId:
				WALK(tree->simpleTemplateId.templateArgumentList);
				WALK(tree->simpleTemplateId.templateName);
				break;
			case AstNodeType::LiteralOperatorTemplateId:
				WALK(tree->literalOperatorTemplateId.literalOperatorId);
				WALK(tree->literalOperatorTemplateId.templateArgumentList);
				break;
			case AstNodeType::FunctionOperatorTemplateId:
				WALK(tree->functionOperatorTemplateId.operatorFunctionId);
				WALK(tree->functionOperatorTemplateId.templateArgumentList);
				break;
			case AstNodeType::TemplateName:
				break;
			case AstNodeType::TypenameSpecifier:
				WALK(tree->typenameSpecifier.nestedNameSpecifier);
				break;
			case AstNodeType::TypenameTemplateSpecifier:
				WALK(tree->typenameTemplateSpecifier.nestedNameSpecifier);
				WALK(tree->typenameTemplateSpecifier.simpleTemplateId);
				break;
			case AstNodeType::ExplicitInstantiation:
				WALK(tree->explicitInstantiation.declaration);
				break;
			case AstNodeType::TryBlock:
				WALK(tree->tryBlock.compoundStatement);
				WALK(tree->tryBlock.handlerSeq);
				break;
			case AstNodeType::FunctionTryBlock:
				WALK(tree->functionTryBlock.compoundStatement);
				WALK(tree->functionTryBlock.ctorInitializer);
				WALK(tree->functionTryBlock.handlerSeq);
				break;
			case AstNodeType::HandlerSeq:
				WALK(tree->handlerSeq.thisHandler);
				WALK(tree->handlerSeq.nextHandler);
				break;
			case AstNodeType::Handler:
				WALK(tree->handler.compoundStatement);
				WALK(tree->handler.exceptionDeclaration);
				break;
			case AstNodeType::ExceptionDeclaration:
				WALK(tree->exceptionDeclaration.attributeSpecifierSeq);
				WALK(tree->exceptionDeclaration.declarator);
				WALK(tree->exceptionDeclaration.typeSpecifierSeq);
				break;
			case AstNodeType::ExceptionAbstractDeclaration:
				WALK(tree->exceptionAbstractDeclaration.abstractDeclarator);
				WALK(tree->exceptionAbstractDeclaration.attributeSpecifierSeq);
				WALK(tree->exceptionAbstractDeclaration.typeSpecifierSeq);
				break;
			case AstNodeType::ThrowExpression:
				WALK(tree->throwExpression.assignmentExpression);
				break;
			case AstNodeType::DynamicExceptionSpec:
				WALK(tree->dynamicExceptionSpec.typeIdList);
				break;
			case AstNodeType::TypeIdList:
				WALK(tree->typeIdList.thisTypeId);
				WALK(tree->typeIdList.nextTypeId);
				break;
			case AstNodeType::NoexceptSpec:
				break;
			case AstNodeType::NoexceptExpressionSpec:
				WALK(tree->noexceptExpression.constantExpression);
				break;
			case AstNodeType::NoptrAbstractDeclarator:
				WALK(tree->noptrAbstractDeclarator.noptrAbstractDeclarator);
				WALK(tree->noptrAbstractDeclarator.parametersAndQualifiers);
				WALK(tree->noptrAbstractDeclarator.ptrAbstractDeclarator);
				break;
			case AstNodeType::NoptrAbstractExpressionDeclarator:
				WALK(tree->noptrAbstractExpressionDeclarator.attributeSpecifierSeq);
				WALK(tree->noptrAbstractExpressionDeclarator.constantExpression);
				WALK(tree->noptrAbstractExpressionDeclarator.noptrAbstractDeclarator);
				WALK(tree->noptrAbstractExpressionDeclarator.ptrAbstractDeclarator);
				break;
			case AstNodeType::UnqualifiedId:
				break;
			case AstNodeType::UnqualifiedIdDtorClass:
				WALK(tree->unqualifiedIdDtorClass.className);
				break;
			case AstNodeType::UnqualifiedIdDtorDecltype:
				WALK(tree->unqualifiedIdDtorDecltype.decltypeSpecifier);
				break;
			case AstNodeType::ElaboratedSpecifierEnum:
				WALK(tree->elaboratedSpecifierEnum.nestedNameSpecifier);
				break;
			case AstNodeType::ElaboratedSpecifierClass:
				WALK(tree->elaboratedSpecifierClass.attributeSpecifierSeq);
				WALK(tree->elaboratedSpecifierClass.classKey);
				WALK(tree->elaboratedSpecifierClass.nestedNameSpecifier);
				break;
			case AstNodeType::ElaboratedSpecifierTemplate:
				WALK(tree->elaboratedSpecifierTemplate.classKey);
				WALK(tree->elaboratedSpecifierTemplate.nestedNameSpecifier);
				WALK(tree->elaboratedSpecifierTemplate.simpleTemplateId);
				break;
			case AstNodeType::AlignmentExpression:
				WALK(tree->alignmentExpression.typeId);
				break;
			case AstNodeType::NoPtrParenDeclarator:
				WALK(tree->noPtrParenDeclarator.ptrDeclarator);
				break;
			case AstNodeType::NoPtrBracketDeclarator:
				WALK(tree->noPtrBracketDeclarator.attributeSpecifierSeq);
				WALK(tree->noPtrBracketDeclarator.constantExpression);
				WALK(tree->noPtrBracketDeclarator.noptrDeclarator);
				break;
			case AstNodeType::NoPtrParamAndQualDeclarator:
				WALK(tree->noPtrParamAndQualDeclarator.noptrDeclarator);
				WALK(tree->noPtrParamAndQualDeclarator.parametersAndQualifiers);
				break;
			case AstNodeType::NoPtrDeclarator:
				WALK(tree->noPtrDeclarator.attributeSpecifierSeq);
				WALK(tree->noPtrDeclarator.declaratorId);
				break;
			case AstNodeType::USystemDeclaration:
				WALK(tree->uSystemDeclaration.namedNamespaceDefinition);
				WALK(tree->uSystemDeclaration.parameterDeclarationClause);
				break;
			case AstNodeType::TypeTemplateParameter:
				WALK(tree->typeTemplateParameter.idExpression);
				WALK(tree->typeTemplateParameter.templateParameterList);
				break;
			case AstNodeType::TypeTypenameParameter:
				WALK(tree->typeTypenameParameter.typeId);
				break;
			case AstNodeType::TypeClassParameter:
				WALK(tree->typeClassParameter.typeId);
				break;
			case AstNodeType::Empty:
				break;
			case AstNodeType::None:
				break;
			case AstNodeType::All:
				break;
			default:
				Logger::Error("Unknown tree node type: '%d' while walking tree.", (int)tree->type);
				break;
			}

			if (postTraversalCallback && (notificationType == AstNodeType::All || tree->type == notificationType))
			{
				callbackFn(tree, userData);
			}
#undef WALK
		}

		void WalkPreprocessingTree(PreprocessingAstNode* tree, void* userData, AstWalkPpTreeUserDataCallbackFn callbackFn,
			PreprocessingAstNodeType notificationType, bool postTraversalCallback)
		{
			if (!postTraversalCallback && (notificationType == PreprocessingAstNodeType::All || tree->type == notificationType))
			{
				callbackFn(tree, userData);
			}

#define WALK(node) WalkPreprocessingTree(node, userData, callbackFn, notificationType, postTraversalCallback)
			switch (tree->type)
			{
			case PreprocessingAstNodeType::PreprocessingFile:
				WALK(tree->preprocessingFile.group);
				break;
			case PreprocessingAstNodeType::Group:
				WALK(tree->group.thisGroupPart);
				WALK(tree->group.nextGroupPart);
				break;
			case PreprocessingAstNodeType::IfSection:
				WALK(tree->ifSection.ifGroup);
				WALK(tree->ifSection.elifGroups);
				WALK(tree->ifSection.elseGroup);
				break;
			case PreprocessingAstNodeType::IfGroup:
				// TODO: Should we walk the constant expression here?
				//WALK(tree->ifGroup.constantExpression);
				WALK(tree->ifGroup.group);
				break;
			case PreprocessingAstNodeType::IfDefGroup:
				WALK(tree->ifDefGroup.group);
				break;
			case PreprocessingAstNodeType::IfNDefGroup:
				WALK(tree->ifNDefGroup.group);
				break;
			case PreprocessingAstNodeType::ElifGroups:
				WALK(tree->elifGroups.thisElifGroup);
				WALK(tree->elifGroups.nextElifGroup);
				break;
			case PreprocessingAstNodeType::ElifGroup:
				// TODO: Same thing here?
				//WALK(tree->elifGroup.constantExpression);
				WALK(tree->elifGroup.group);
				break;
			case PreprocessingAstNodeType::ElseGroup:
				WALK(tree->elseGroup.group);
				break;
			case PreprocessingAstNodeType::MacroInclude:
				WALK(tree->macroInclude.ppTokens);
				break;
			case PreprocessingAstNodeType::MacroDefine:
				WALK(tree->macroDefine.replacementList);
				break;
			case PreprocessingAstNodeType::MacroDefineFunction:
				WALK(tree->macroDefineFunction.replacementList);
				WALK(tree->macroDefineFunction.identifierList);
				break;
			case PreprocessingAstNodeType::MacroUndef:
				break;
			case PreprocessingAstNodeType::MacroLine:
				WALK(tree->macroLine.ppTokens);
				break;
			case PreprocessingAstNodeType::MacroError:
				WALK(tree->macroError.ppTokens);
				break;
			case PreprocessingAstNodeType::MacroPragma:
				WALK(tree->macroPragma.ppTokens);
				break;
			case PreprocessingAstNodeType::TextLine:
				WALK(tree->textLine.ppTokens);
				break;
			case PreprocessingAstNodeType::NonDirective:
				WALK(tree->nonDirective.ppTokens);
				break;
			case PreprocessingAstNodeType::Identifier:
				break;
			case PreprocessingAstNodeType::IdentifierList:
				WALK(tree->identifierList.thisIdentifierNode);
				WALK(tree->identifierList.nextIdentifierNode);
				break;
			case PreprocessingAstNodeType::ReplacementList:
				WALK(tree->replacementList.ppTokens);
				break;
			case PreprocessingAstNodeType::PPTokens:
				WALK(tree->ppTokens.preprocessingToken);
				WALK(tree->ppTokens.nextPreprocessingToken);
				break;
			case PreprocessingAstNodeType::NumberLiteral:
				break;
			case PreprocessingAstNodeType::StringLiteral:
				break;
			case PreprocessingAstNodeType::CharacterLiteral:
				break;
			case PreprocessingAstNodeType::HeaderName:
				break;
			case PreprocessingAstNodeType::HeaderNameString:
				break;
			case PreprocessingAstNodeType::EmptyMacro:
				break;
			case PreprocessingAstNodeType::PreprocessingOpOrPunc:
				break;
			case PreprocessingAstNodeType::All:
				break;
			case PreprocessingAstNodeType::None:
				break;
			default:
				Logger::Error("Unknown tree node type: '%d' while walking preprocessing tree.", (int)tree->type);
				break;
			}

			if (postTraversalCallback && (notificationType == PreprocessingAstNodeType::All || tree->type == notificationType))
			{
				callbackFn(tree, userData);
			}
#undef WALK
		}

		// ===============================================================================================
		// Helper functions (internal)
		//
		// These are a collection of functions used to give useful information during parse
		// ===============================================================================================
		static bool AtEnd(ParserData& data)
		{
			return data.CurrentToken >= data.Tokens.size();
		}

		static void ErrorAtToken(ParserData& data)
		{
			Token& currentToken = AtEnd(data) ? data.Tokens[data.Tokens.size() - 1] : data.Tokens[data.CurrentToken];
			Logger::Error("Unexpected token '%s' at line %d:%d", ScriptScanner::TokenName(currentToken.m_Type), currentToken.m_Line, currentToken.m_Column);
		}

		static void Consume(ParserData& data, TokenType type)
		{
			Token& currentToken = data.Tokens[data.CurrentToken];
			if (currentToken.m_Type == type)
			{
				data.CurrentToken++;
				return;
			}

			Logger::Error("Unexpected token. Expected '%s' instead got '%s' at line: %d:%d", ScriptScanner::TokenName(type),
				ScriptScanner::TokenName(currentToken.m_Type), currentToken.m_Line, currentToken.m_Column);
		}

		static void BacktrackTo(ParserData& data, int position)
		{
			if (!(position >= 0 && position < data.Tokens.size()))
			{
				// TODO: Remove me!
				printf("HERE");
			}
			Logger::Assert(position >= 0 && position < data.Tokens.size(), "Invalid backtrack location.");
			data.CurrentToken = position;
		}

		static bool Match(ParserData& data, TokenType type)
		{
			if (AtEnd(data))
			{
				return false;
			}
			Token& currentToken = data.Tokens[data.CurrentToken];
			if (currentToken.m_Type == type)
			{
				data.CurrentToken++;
				return true;
			}

			return false;
		}

		static Token ConsumeCurrent(ParserData& data, TokenType type)
		{
			Token& currentToken = AtEnd(data) ? data.Tokens[data.Tokens.size() - 1] : data.Tokens[data.CurrentToken];
			if (currentToken.m_Type == type)
			{
				data.CurrentToken++;
				return currentToken;
			}

			Logger::Error("Unexpected token. Expected '%s' instead got '%s' at line %d:%d",
				ScriptScanner::TokenName(type),
				ScriptScanner::TokenName(currentToken.m_Type),
				currentToken.m_Line,
				currentToken.m_Column);
			return currentToken;
		}

		static Token GetCurrentToken(ParserData& data)
		{
			return AtEnd(data) ? data.Tokens[data.Tokens.size() - 1] : data.Tokens[data.CurrentToken];
		}

		static bool IsAssignmentOperator(TokenType type)
		{
			return type == TokenType::EQUAL || type == TokenType::STAR_EQUAL || type == TokenType::DIV_EQUAL || type == TokenType::MODULO_EQUAL ||
				type == TokenType::PLUS_EQUAL || type == TokenType::MINUS_EQUAL || type == TokenType::RIGHT_SHIFT_EQUAL || type == TokenType::LEFT_SHIFT_EQUAL ||
				type == TokenType::AND_EQUAL || type == TokenType::CARET_EQUAL || type == TokenType::PIPE_EQUAL;
		}

		static TokenType Peek(ParserData& data)
		{
			return AtEnd(data) ? data.Tokens[data.Tokens.size() - 1].m_Type : data.Tokens[data.CurrentToken].m_Type;
		}

		static bool PeekIn(ParserData& data, std::initializer_list<TokenType> tokenTypes)
		{
			if (std::find(tokenTypes.begin(), tokenTypes.end(), Peek(data)) != tokenTypes.end())
			{
				return true;
			}
			return false;
		}

		static bool LookAheadBeforeSemicolon(ParserData& data, std::initializer_list<TokenType> tokenTypes)
		{
			// This function looks for a token that matches any of the types in the initializer list
			// before the first semicolon or eof token. If it finds it, it returns true, otherwise false
			int token = data.CurrentToken;
			size_t tokenTypeSize = tokenTypes.size();
			while (!AtEnd(data))
			{
				Token& iter = data.Tokens[token];
				if (iter.m_Type == TokenType::SEMICOLON)
				{
					return false;
				}

				if (std::find(tokenTypes.begin(), tokenTypes.end(), iter.m_Type) != tokenTypes.end())
				{
					return true;
				}
				token++;
				if (token >= data.Tokens.size())
				{
					return false;
				}
			}

			return false;
		}

		static bool MatchBeforeSemicolon(ParserData& data, TokenType type1, TokenType nextType)
		{
			// This function looks for a token that matches any of the types in the initializer list
			// before the first semicolon or eof token. If it finds it, it returns true, otherwise false
			int token = data.CurrentToken;
			while (!AtEnd(data))
			{
				Token& iter = data.Tokens[token];
				if (iter.m_Type == TokenType::SEMICOLON)
				{
					return false;
				}

				if (iter.m_Type == type1 && token < data.Tokens.size() && data.Tokens[token + 1].m_Type == nextType)
				{
					return true;
				}
				token++;
				if (token >= data.Tokens.size())
				{
					return false;
				}
			}

			return false;
		}

		// ===============================================================================================
		// Generator functions (internal)
		//
		// These are a collection of functions that are used to construct any of the AST node types
		// ===============================================================================================
		static AstNode* GenerateAstNode(AstNodeType type)
		{
			AstNode* node = (AstNode*)AllocMem(sizeof(AstNode));
			node->success = true;
			node->type = type;
			return node;
		}

		static PreprocessingAstNode* GeneratePreprocessingAstNode(PreprocessingAstNodeType type)
		{
			PreprocessingAstNode* node = (PreprocessingAstNode*)AllocMem(sizeof(PreprocessingAstNode));
			node->success = true;
			node->type = type;
			return node;
		}

		static AstNode* GenerateEmptyStatementNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::Empty);
			return result;
		}

		static AstNode* GenerateBinaryExpressionNode(AstNode* left, OverloadableOperatorType op, AstNode* right)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BinaryExpression);
			result->binaryExpression.left = left;
			result->binaryExpression.opType = op;
			result->binaryExpression.right = right;
			return result;
		}

		static AstNode* GenerateTernaryExpressionNode(AstNode* comparisonExpression, AstNode* ifTrueNode, AstNode* ifFalseNode)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TernaryExpression);
			result->ternaryExpression.comparisonExpression = comparisonExpression;
			result->ternaryExpression.ifTrueNode = ifTrueNode;
			result->ternaryExpression.ifFalseNode = ifFalseNode;
			return result;
		}

		static AstNode* GenerateAssignmentExpressionNode(AstNode* leftSide, AssignmentOperatorType opType, AstNode* initializerClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AssignmentExpression);
			result->assignmentExpression.leftSide = leftSide;
			result->assignmentExpression.opType = opType;
			result->assignmentExpression.initializerClause = initializerClause;
			return result;
		}

		static AstNode* GeneratePointerToMemberNode(AstNode* left, AstNode* right)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PointerToMember);
			result->pointerToMember.left = left;
			result->pointerToMember.right = right;
			return result;
		}

		static AstNode* GenerateCastExpressionNode(AstNode* typeId, AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CastExpression);
			result->castExpression.typeId = typeId;
			result->castExpression.expression = expression;
			return result;
		}

		static AstNode* GenerateUnaryExpressionNode(OverloadableOperatorType opType, AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UnaryExpression);
			result->unaryExpression.opType = opType;
			result->unaryExpression.expression = expression;
			return result;
		}

		static AstNode* GenerateSizeofExpressionNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SizeofExpression);
			result->sizeofExpression.expression = expression;
			return result;
		}

		static AstNode* GenerateSizeofIdentifierExpressionNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SizeofIdentifierExpression);
			result->sizeofIdentifierExpression.identifier = identifier;
			return result;
		}

		static AstNode* GenerateAlignofExpressionNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AlignofExpression);
			result->alignofExpression.expression = expression;
			return result;
		}

		static AstNode* GenerateDeleteNode(AstNode* expression, bool deleteArray)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Delete);
			result->deleteNode.expression = expression;
			result->deleteNode.deleteArray = deleteArray;
			return result;
		}

		static AstNode* GenerateLiteralNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Literal);
			result->literalNode.token = token;
			return result;
		}

		static AstNode* GenerateThisNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::This);
			result->thisNode.token = token;
			return result;
		}

		static AstNode* GenerateGroupingNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Grouping);
			result->grouping.expression = expression;
			return result;
		}

		static AstNode* GenerateLambdaExpressionNode(AstNode* introducer, AstNode* declarator, AstNode* compoundStatement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LambdaExpression);
			result->lambdaExpression.introducer = introducer;
			result->lambdaExpression.declarator = declarator;
			result->lambdaExpression.compoundStatement = compoundStatement;
			return result;
		}

		static AstNode* GenerateLambdaIntroducerNode(AstNode* capture)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LambdaIntroducer);
			result->lambdaIntroducer.lambdaCapture = capture;
			return result;
		}

		static AstNode* GenerateLambdaCaptureNode(Token captureDefault, AstNode* captureList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LambdaCapture);
			result->lambdaCapture.hasDefaultRef = captureDefault.m_Type == TokenType::AND;
			result->lambdaCapture.hasDefaultCopy = captureDefault.m_Type == TokenType::EQUAL;
			result->lambdaCapture.captureList = captureList;
			return result;
		}

		static AstNode* GenerateCaptureNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Capture);
			result->capture.identifier = identifier;
			return result;
		}

		static AstNode* GenerateLambdaCaptureListNode(AstNode* thisCapture, AstNode* nextCapture)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LambdaCaptureList);
			result->lambdaCaptureList.thisCapture = thisCapture;
			result->lambdaCaptureList.nextCapture = nextCapture;
			return result;
		}

		static AstNode* GenerateLambdaDeclaratorNode(AstNode* parameterDeclarationClause, bool isMutable, AstNode* exceptionSpec, AstNode* attributeSpec, AstNode* trailingReturnType)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LambdaDeclarator);
			result->lambdaDeclarator.parameterDeclarationClause = parameterDeclarationClause;
			result->lambdaDeclarator.exceptionSpecification = exceptionSpec;
			result->lambdaDeclarator.attributeSpecifierSequence = attributeSpec;
			result->lambdaDeclarator.trailingReturnType = trailingReturnType;
			result->lambdaDeclarator.isMutable = isMutable;
			return result;
		}

		static AstNode* GenerateOperatorFunctionIdNode(OverloadableOperatorType opType, AstNode* templateArgList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::OperatorFunctionId);
			result->operatorFunctionId.opType = opType;
			result->operatorFunctionId.templateArgList = templateArgList;
			return result;
		}

		static AstNode* GenerateTemplateArgumentListNode(AstNode* thisArgument, AstNode* nextArgument)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TemplateArgumentList);
			result->templateArgumentList.thisArgument = thisArgument;
			result->templateArgumentList.nextArgument = nextArgument;
			return result;
		}

		static AstNode* GenerateTemplateQualifiedIdNode(AstNode* nestedNamespaceSpecifier, bool hasNamespaceScope, bool hasTemplateKeyword)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TemplateQualifiedId);
			result->templateQualifiedId.nestedNamespaceSpecifier = nestedNamespaceSpecifier;
			result->templateQualifiedId.hasNamespaceScope = hasNamespaceScope;
			result->templateQualifiedId.hasTemplateKeyword = hasTemplateKeyword;
			return result;
		}

		static AstNode* GenerateTypeIdNode(AstNode* typeSpecifierSeq, AstNode* abstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeId);
			result->typeIdNode.typeSpecifierSeq = typeSpecifierSeq;
			result->typeIdNode.abstractDeclarator = abstractDeclarator;
			return result;
		}

		static AstNode* GenerateEnumNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumName);
			result->enumName.identifier = identifier;
			return result;
		}

		static AstNode* GenerateEnumSpecifierNode(AstNode* enumHead, AstNode* enumeratorList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumSpecifier);
			result->enumSpecifier.enumHead = enumHead;
			result->enumSpecifier.enumeratorList = enumeratorList;
			return result;
		}

		static AstNode* GenerateEnumKeyNode(EnumKeyType type)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumKey);
			result->enumKey.type = type;
			return result;
		}

		static AstNode* GenerateEnumHeadNode(AstNode* enumKey, AstNode* attributeSpecifierSeq, AstNode* nestedNameSpecifier, Token identifier, AstNode* enumBase)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumHead);
			result->enumHead.enumKey = enumKey;
			result->enumHead.attributeSpecifierSeq = attributeSpecifierSeq;
			result->enumHead.nestedNameSpecifier = nestedNameSpecifier;
			result->enumHead.identifier = identifier;
			result->enumHead.enumBase = enumBase;
			return result;
		}

		static AstNode* GenerateOpaqueEnumDeclNode(AstNode* enumKey, AstNode* attributeSpecifierSeq, Token identifier, AstNode* enumBase)
		{
			AstNode* result = GenerateAstNode(AstNodeType::OpaqueEnumDecl);
			result->opaqueEnumDecl.enumKey = enumKey;
			result->opaqueEnumDecl.attributeSpecifierSeq = attributeSpecifierSeq;
			result->opaqueEnumDecl.identifier = identifier;
			result->opaqueEnumDecl.enumBase = enumBase;
			return result;
		}

		static AstNode* GenerateEnumBaseNode(AstNode* typeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumBase);
			result->enumBase.TypeSpecifierSeq = typeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateEnumeratorListNode(AstNode* enumDefinition, AstNode* nextEnumDefinition)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumeratorList);
			result->enumeratorList.enumDefinition = enumDefinition;
			result->enumeratorList.nextEnumDefinition = nextEnumDefinition;
			return result;
		}

		static AstNode* GenerateEnumeratorDefinitionNode(Token identifier, AstNode* value)
		{
			AstNode* result = GenerateAstNode(AstNodeType::EnumeratorDefinition);
			result->enumeratorDefinition.identifier = identifier;
			result->enumeratorDefinition.value = value;
			return result;
		}

		static AstNode* GenerateConstantExpressionNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ConstantExpression);
			result->constantExpression.expression = expression;
			return result;
		}

		static AstNode* GenerateIfElseNode(AstNode* condition, AstNode* ifStatement, AstNode* elseStatement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IfElse);
			result->ifElseNode.condition = condition;
			result->ifElseNode.ifStatement = ifStatement;
			result->ifElseNode.elseStatement = elseStatement;
			return result;
		}

		static AstNode* GenerateSwitchNode(AstNode* condition, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Switch);
			result->switchNode.condition = condition;
			result->switchNode.statement = statement;
			return result;
		}

		static AstNode* GenerateInitializerConditionNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator, AstNode* initializerClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::InitializerCondition);
			result->initCondition.attributeSpecifierSeq = attributeSpecifierSeq;
			result->initCondition.declSpecifierSeq = declSpecifierSeq;
			result->initCondition.declarator = declarator;
			result->initCondition.initializerClause = initializerClause;
			return result;
		}

		static AstNode* GenerateBracedInitConditionNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BracedInitCondition);
			result->bracedInitCondition.attributeSpecifierSeq = attributeSpecifierSeq;
			result->bracedInitCondition.declSpecifierSeq = declSpecifierSeq;
			result->bracedInitCondition.declarator = declarator;
			result->bracedInitCondition.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GenerateWhileLoopNode(AstNode* condition, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::WhileLoop);
			result->whileLoopNode.condition = condition;
			result->whileLoopNode.statement = statement;
			return result;
		}

		static AstNode* GenerateDoWhileLoopNode(AstNode* statement, AstNode* condition)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DoWhileLoop);
			result->whileLoopNode.statement = statement;
			result->whileLoopNode.condition = condition;
			return result;
		}

		static AstNode* GenerateForLoopNode(AstNode* forInitStatement, AstNode* condition, AstNode* expression, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ForLoop);
			result->forLoopNode.forInitStatement = forInitStatement;
			result->forLoopNode.condition = condition;
			result->forLoopNode.expression = expression;
			result->forLoopNode.statement = statement;
			return result;
		}

		static AstNode* GenerateForEachLoopNode(AstNode* forRangeDeclaration, AstNode* forRangeInitializer, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ForEachLoop);
			result->forEachLoopNode.forRangeDeclaration = forRangeDeclaration;
			result->forEachLoopNode.forRangeInitializer = forRangeInitializer;
			result->forEachLoopNode.statement = statement;
			return result;
		}

		static AstNode* GenerateForRangeDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* typeSpecifierSeq, AstNode* declarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ForRangeDeclaration);
			result->forRangeDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->forRangeDeclaration.typeSpecifierSeq = typeSpecifierSeq;
			result->forRangeDeclaration.declarator = declarator;
			return result;
		}

		static AstNode* GenerateForRangeInitializerNode(AstNode* expression, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ForRangeInitializer);
			result->forRangeInitializer.expression = expression;
			result->forRangeInitializer.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GenerateStatementNode(AstNode* attributeSpecifierSeq, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Statement);
			result->statement.attributeSpecifierSeq = attributeSpecifierSeq;
			result->statement.statement = statement;
			return result;
		}

		static AstNode* GenerateQualifiedIdNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::QualifiedId);
			result->qualifeidId.identifier = identifier;
			return result;
		}

		static AstNode* GenerateLabeledIdentifierNode(AstNode* attributeSpecifierSeq, Token identifier, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LabeledIdentifier);
			result->labeledIdentifier.attributeSpecifierSeq = attributeSpecifierSeq;
			result->labeledIdentifier.identifier = identifier;
			result->labeledIdentifier.statement = statement;
			return result;
		}

		static AstNode* GenerateCaseLabelNode(AstNode* attributeSpecifierSeq, AstNode* constantExpression, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CaseLabel);
			result->caseLabel.attributeSpecifierSeq = attributeSpecifierSeq;
			result->caseLabel.constantExpression = constantExpression;
			result->caseLabel.statement = statement;
			return result;
		}

		static AstNode* GenerateDefaultLabelNode(AstNode* attributeSpecifierSeq, AstNode* statement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DefaultLabel);
			result->defaultLabel.attributeSpecifierSeq = attributeSpecifierSeq;
			result->defaultLabel.statement = statement;
			return result;
		}

		static AstNode* GenerateCompoundStatementNode(AstNode* statementSequence)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CompoundStatement);
			result->compoundStatement.statementSequence = statementSequence;
			return result;
		}

		static AstNode* GenerateStatementSequenceNode(AstNode* thisStatement, AstNode* nextStatement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::StatementSequence);
			result->statementSequence.thisStatement = thisStatement;
			result->statementSequence.nextStatement = nextStatement;
			return result;
		}

		static AstNode* GenerateBreakNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::Break);
			return result;
		}

		static AstNode* GenerateContinueNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::Continue);
			return result;
		}

		static AstNode* GenerateReturnNode(AstNode* returnValue)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Return);
			result->returnNode.returnValue = returnValue;
			return result;
		}

		static AstNode* GenerateGotoNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Goto);
			result->gotoNode.identifier = identifier;
			return result;
		}

		static AstNode* GenerateNamespaceNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NamespaceName);
			result->namespaceNameNode.identifier = identifier;
			return result;
		}

		static AstNode* GenerateNamedNamespaceDefinitionNode(bool isInline, Token identifier, AstNode* namespaceBody)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NamedNamespaceDefinition);
			result->namedNamespaceDefinition.isInline = isInline;
			result->namedNamespaceDefinition.identifier = identifier;
			result->namedNamespaceDefinition.namespaceBody = namespaceBody;
			return result;
		}

		static AstNode* GenerateUnnamedNamespaceDefinitionNode(bool isInline, AstNode* namespaceBody)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UnnamedNamespaceDefinition);
			result->unnamedNamespaceDefinition.isInline = isInline;
			result->unnamedNamespaceDefinition.namespaceBody = namespaceBody;
			return result;
		}

		static AstNode* GenerateNamespaceAliasDefinitionNode(Token identifier, AstNode* qualifiedNamespaceSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NamespaceAliasDefinition);
			result->namespaceAliasDefinition.identifier = identifier;
			result->namespaceAliasDefinition.qualifiedNamespaceSpecifier = qualifiedNamespaceSpecifier;
			return result;
		}

		static AstNode* GenerateQualifiedNamespaceSpecifierNode(bool isNested, AstNode* nestedNameSpecifier, AstNode* namespaceName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::QualifiedNamespaceSpecifier);
			result->qualifiedNamespaceSpecifier.isNested = isNested;
			result->qualifiedNamespaceSpecifier.nestedNameSpecifier = nestedNameSpecifier;
			result->qualifiedNamespaceSpecifier.namespaceName = namespaceName;
			return result;
		}

		static AstNode* GenerateUsingDeclarationNode(AstNode* unqualifiedId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UsingDeclaration);
			result->usingDeclaration.unqualifiedId = unqualifiedId;
			return result;
		}

		static AstNode* GenerateUsingTypenameDeclarationNode(bool hasTypename, bool isNested, AstNode* nestedNamespaceSpecifier, AstNode* unqualifiedId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UsingTypenameDeclaration);
			result->usingTypenameDeclaration.hasTypename = hasTypename;
			result->usingTypenameDeclaration.isNested = isNested;
			result->usingTypenameDeclaration.nestedNameSpecifier = nestedNamespaceSpecifier;
			result->usingTypenameDeclaration.unqualifiedId = unqualifiedId;
			return result;
		}

		static AstNode* GenerateUsingDirectiveNode(AstNode* attributeSpecifierSeq, bool isNested, AstNode* nestedNameSpecifier, AstNode* namespaceName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UsingDirective);
			result->usingDirective.attributeSpecifierSeq = attributeSpecifierSeq;
			result->usingDirective.isNested = isNested;
			result->usingDirective.nestedNameSpecifier = nestedNameSpecifier;
			result->usingDirective.namespaceName = namespaceName;
			return result;
		}

		static AstNode* GenerateAsmNode(Token stringLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Asm);
			result->asmNode.stringLiteral = stringLiteral;
			return result;
		}

		static AstNode* GenerateLinkageSpecificationBlockNode(Token stringLiteral, AstNode* declarationSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LinkageSpecificationBlock);
			result->linkageSpecificationBlock.stringLiteral = stringLiteral;
			result->linkageSpecificationBlock.declarationSeq = declarationSeq;
			return result;
		}

		static AstNode* GenerateLinkageSpecificationNode(Token stringLiteral, AstNode* declaration)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LinkageSpecification);
			result->linkageSpecification.stringLiteral = stringLiteral;
			result->linkageSpecification.declaration = declaration;
			return result;
		}

		static AstNode* GenerateAttributeSpecifierSequenceNode(AstNode* thisSpec, AstNode* nextSpec)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AttributeSpecifierSequence);
			result->attributeSpecifierSeq.thisSpec = thisSpec;
			result->attributeSpecifierSeq.nextSpec = nextSpec;
			return result;
		}

		static AstNode* GenerateAlignAsExpressionNode(AstNode* alignmentExpression, bool hasElipsis)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AlignAsExpression);
			result->alignAsExpression.alignmentExpression = alignmentExpression;
			result->alignAsExpression.hasElipsis = hasElipsis;
			return result;
		}

		static AstNode* GenerateAlignAsTypeIdNode(AstNode* typeId, bool hasElipsis)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AlignAsTypeId);
			result->alignAsTypeId.typeId = typeId;
			result->alignAsTypeId.hasElipsis = hasElipsis;
			return result;
		}

		static AstNode* GenerateAttributeListNode(AstNode* thisAttribute, AstNode* nextAttribute)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AttributeList);
			result->attributeList.thisAttribute = thisAttribute;
			result->attributeList.nextAttribute = nextAttribute;
			return result;
		}

		static AstNode* GenerateEmptyAttributeListNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::EmptyAttributeList);
			return result;
		}

		static AstNode* GenerateAttributeNode(AstNode* attributeToken, AstNode* attributeArgumentClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Attribute);
			result->attribute.attributeToken = attributeToken;
			result->attribute.attributeArgumentClause = attributeArgumentClause;
			return result;
		}

		static AstNode* GenerateAttributeTokenNode(Token namespaceName, Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AttributeToken);
			result->attributeToken.namespaceName = namespaceName;
			result->attributeToken.identifier = identifier;
			return result;
		}

		static AstNode* GenerateAttributeArgumentClauseNode(AstNode* balancedTokenSequence)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AttributeArgumentClause);
			result->attributeArgumentClause.balancedTokenSequence = balancedTokenSequence;
			return result;
		}

		static AstNode* GenerateBalancedTokenSeqNode(AstNode* thisToken, AstNode* nextToken)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BalancedTokenSeq);
			result->balancedTokenSeq.thisToken = thisToken;
			result->balancedTokenSeq.nextToken = nextToken;
			return result;
		}

		static AstNode* GenerateBalancedTokenNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BalancedToken);
			result->balancedToken.token = token;
			return result;
		}

		static AstNode* GenerateInitDeclaratorListNode(AstNode* thisDeclarator, AstNode* nextDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::InitDeclaratorList);
			result->initDeclaratorList.thisDeclarator = thisDeclarator;
			result->initDeclaratorList.nextDeclarator = nextDeclarator;
			return result;
		}

		static AstNode* GenerateInitDeclaratorNode(AstNode* declarator, AstNode* initializer)
		{
			AstNode* result = GenerateAstNode(AstNodeType::InitDeclarator);
			result->initDeclarator.declarator = declarator;
			result->initDeclarator.initializer = initializer;
			return result;
		}

		static AstNode* GenerateDeclaratorNode(AstNode* noPtrDeclarator, AstNode* parametersAndQualifiers, AstNode* trailingReturnType)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Declarator);
			result->declaratorNode.noPtrDeclarator = noPtrDeclarator;
			result->declaratorNode.parametersAndQualifiers = parametersAndQualifiers;
			result->declaratorNode.trailingReturnType = trailingReturnType;
			return result;
		}

		static AstNode* GeneratePtrDeclaratorNode(AstNode* ptrOperator, AstNode* ptrDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PtrDeclarator);
			result->ptrDeclarator.ptrOperator = ptrOperator;
			result->ptrDeclarator.ptrDeclarator = ptrDeclarator;
			return result;
		}

		static AstNode* GenerateParametersAndQualifiersNode(AstNode* parameterDeclarationClause,
			AstNode* attributeSpecifierSeq, AstNode* cvQualifierSeq, AstNode* refQualifier, AstNode* exceptionSpecification)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParametersAndQualifiers);
			result->parametersAndQualifiersNode.parameterDeclarationClause = parameterDeclarationClause;
			result->parametersAndQualifiersNode.attributeSpecifierSeq = attributeSpecifierSeq;
			result->parametersAndQualifiersNode.cvQualifierSeq = cvQualifierSeq;
			result->parametersAndQualifiersNode.refQualifier = refQualifier;
			result->parametersAndQualifiersNode.exceptionSpecification = exceptionSpecification;
			return result;
		}

		static AstNode* GenerateTrailingReturnTypeNode(AstNode* trailingTypeSpecifierSeq, AstNode* abstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TrailingReturnType);
			result->trailingReturnTypeNode.trailingTypeSpecifierSeq = trailingTypeSpecifierSeq;
			result->trailingReturnTypeNode.abstractDeclarator = abstractDeclarator;
			return result;
		}

		static AstNode* GeneratePtrNamespaceStarNode(AstNode* nestedNameSpecifier, AstNode* attributeSpecifierSeq, AstNode* cvQualifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PtrNamespaceStar);
			result->ptrNamespaceStar.nestedNameSpecifier = nestedNameSpecifier;
			result->ptrNamespaceStar.attributeSpecifierSeq = attributeSpecifierSeq;
			result->ptrNamespaceStar.cvQualifierSeq = cvQualifierSeq;
			return result;
		}

		static AstNode* GenerateRefRefNode(AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::RefRef);
			result->refRef.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateRefNode(AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Ref);
			result->ref.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GeneratePtrStarNode(AstNode* attributeSpecifierSeq, AstNode* cvQualifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PtrStar);
			result->ptrStar.attributeSpecifierSeq = attributeSpecifierSeq;
			result->ptrStar.cvQualifierSeq = cvQualifierSeq;
			return result;
		}

		static AstNode* GenerateCvQualifierSeqNode(AstNode* thisQualifier, AstNode* nextQualifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CvQualifierSeq);
			result->cvQualifierSeq.thisQualifier = thisQualifier;
			result->cvQualifierSeq.nextQualifier = nextQualifier;
			return result;
		}

		static AstNode* GenerateCvQualifierNode(Token qualifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CvQualifier);
			result->cvQualifier.qualifier = qualifier;
			return result;
		}

		static AstNode* GenerateRefQualifierNode(bool doubleRef)
		{
			AstNode* result = GenerateAstNode(AstNodeType::RefQualifier);
			result->refQualifier.doubleRef = doubleRef;
			return result;
		}

		static AstNode* GenerateDeclaratorIdNode(AstNode* nestedNameSpecifier, AstNode* className)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DeclaratorId);
			result->declaratorId.nestedNameSpecifier = nestedNameSpecifier;
			result->declaratorId.className = className;
			return result;
		}

		static AstNode* GenerateClassNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassName);
			result->className.identifier = identifier;
			return result;
		}

		static AstNode* GenerateClassSpecifierNode(AstNode* classHead, AstNode* memberSpecification)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassSpecifier);
			result->classSpecifier.classHead = classHead;
			result->classSpecifier.memberSpecification = memberSpecification;
			return result;
		}

		static AstNode* GenerateClassVirtualHeadNode(AstNode* classKey, AstNode* attributeSpecifierSeq, AstNode* classHeadName, AstNode* classVirtSpecifierSeq, AstNode* baseClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassVirtualHead);
			result->classVirtualHead.classKey = classKey;
			result->classVirtualHead.attributeSpecifierSeq = attributeSpecifierSeq;
			result->classVirtualHead.classHeadName = classHeadName;
			result->classVirtualHead.classVirtSpecifierSeq = classVirtSpecifierSeq;
			result->classVirtualHead.baseClause = baseClause;
			return result;
		}

		static AstNode* GenerateClassHeadNode(AstNode* classKey, AstNode* attributeSpecifierSeq, AstNode* baseClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassHead);
			result->classHead.classKey = classKey;
			result->classHead.attributeSpecifierSeq = attributeSpecifierSeq;
			result->classHead.baseClause = baseClause;
			return result;
		}

		static AstNode* GenerateClassHeadNameNode(AstNode* nestedNameSpecifier, AstNode* className)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassHeadName);
			result->classHeadName.nestedNameSpecifier = nestedNameSpecifier;
			result->classHeadName.className = className;
			return result;
		}

		static AstNode* GenerateClassVirtSpecifierSeqNode(AstNode* thisSpec, AstNode* nextSpec)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassVirtSpecifierSeq);
			result->classVirtSpecifierSeq.thisSpec = thisSpec;
			result->classVirtSpecifierSeq.nextSpec = nextSpec;
			return result;
		}

		static AstNode* GenerateClassVirtSpecifierNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassVirtSpecifier);
			result->classVirtSpecifier.token = token;
			return result;
		}

		static AstNode* GenerateClassKeyNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassKey);
			result->classKey.token = token;
			return result;
		}

		static AstNode* GenerateMemberAndAccessSpecifierNode(AstNode* accessSpecifier, AstNode* memberSpecification)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberAndAccessSpecifier);
			result->memberAndAccessSpecifier.accessSpecifier = accessSpecifier;
			result->memberAndAccessSpecifier.memberSpecification = memberSpecification;
			return result;
		}

		static AstNode* GenerateMemberSpecifierNode(AstNode* memberDeclaration, AstNode* memberSpecification)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberSpecifier);
			result->memberSpecifier.memberDeclaration = memberDeclaration;
			result->memberSpecifier.memberSpecification = memberSpecification;
			return result;
		}

		static AstNode* GenerateMemberFunctionDeclarationNode(AstNode* functionDefinition, bool hasTrailingSemicolon)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberFunctionDeclaration);
			result->memberFunctionDeclaration.functionDefinition = functionDefinition;
			result->memberFunctionDeclaration.hasTrailingSemicolon = hasTrailingSemicolon;
			return result;
		}

		static AstNode* GenerateMemberDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* memberDeclaratorList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberDeclaration);
			result->memberDeclarationNode.attribtueSpecifierSeq = attributeSpecifierSeq;
			result->memberDeclarationNode.declSpecifierSeq = declSpecifierSeq;
			result->memberDeclarationNode.memberDeclaratorList = memberDeclaratorList;
			return result;
		}

		static AstNode* GenerateMemberDeclaratorListNode(AstNode* thisDeclarator, AstNode* nextDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberDeclaratorList);
			result->memberDeclaratorList.thisDeclarator = thisDeclarator;
			result->memberDeclaratorList.nextDeclarator = nextDeclarator;
			return result;
		}

		static AstNode* GenerateMemberDeclaratorPureNode(AstNode* declarator, AstNode* virtSpecifierSeq, AstNode* pureSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberDeclaratorPure);
			result->memberDeclaratorPure.declarator = declarator;
			result->memberDeclaratorPure.virtSpecifierSeq = virtSpecifierSeq;
			result->memberDeclaratorPure.pureSpecifier = pureSpecifier;
			return result;
		}

		static AstNode* GenerateMemberDeclaratorBraceNode(AstNode* declarator, AstNode* virtSpecifierSeq, AstNode* braceOrEqualInitializer)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberDeclaratorBrace);
			result->memberDeclaratorBrace.declarator = declarator;
			result->memberDeclaratorBrace.virtSpecifierSeq = virtSpecifierSeq;
			result->memberDeclaratorBrace.braceOrEqualInitializer = braceOrEqualInitializer;
			return result;
		}

		static AstNode* GenerateMemberDeclaratorNode(Token identifier, AstNode* attributeSpecifierSeq, AstNode* virtSpecifierSeq, AstNode* constantExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemberDeclarator);
			result->memberDeclarator.identifier = identifier;
			result->memberDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			result->memberDeclarator.virtSpecifierSeq = virtSpecifierSeq;
			result->memberDeclarator.constantExpression = constantExpression;
			return result;
		}

		static AstNode* GenerateVirtSpecifierSeqNode(AstNode* thisSpec, AstNode* nextSpec)
		{
			AstNode* result = GenerateAstNode(AstNodeType::VirtSpecifierSeq);
			result->virtSpecifierSeq.thisSpec = thisSpec;
			result->virtSpecifierSeq.nextSpec = nextSpec;
			return result;
		}

		static AstNode* GenerateVirtSpecifierNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::VirtSpecifier);
			result->virtSpecifier.token = token;
			return result;
		}

		static AstNode* GeneratePureSpecifierNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::PureSpecifier);
			return result;
		}

		static AstNode* GenerateBaseSpecifierListNode(AstNode* thisBaseSpecifier, AstNode* nextBaseSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BaseSpecifierList);
			result->baseSpecifierList.thisBaseSpecifier = thisBaseSpecifier;
			result->baseSpecifierList.nextBaseSpecifier = nextBaseSpecifier;
			return result;
		}

		static AstNode* GenerateBaseSpecifierNode(AstNode* attributeSpecifierSeq, bool isVirtual, AstNode* accessSpecifier, AstNode* baseTypeSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BaseSpecifier);
			result->baseSpecifier.attributeSpecifierSeq = attributeSpecifierSeq;
			result->baseSpecifier.isVirtual = isVirtual;
			result->baseSpecifier.accessSpecifier = accessSpecifier;
			result->baseSpecifier.baseTypeSpecifier = baseTypeSpecifier;
			return result;
		}

		static AstNode* GenerateClassOrDecltypeNode(AstNode* nestedNameSpecifier, AstNode* className)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ClassOrDecltype);
			result->classOrDecltype.nestedNameSpecifier = nestedNameSpecifier;
			result->classOrDecltype.className = className;
			return result;
		}

		static AstNode* GenerateAccessSpecifierNode(Token accessSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AccessSpecifier);
			result->accessSpecifier.accessSpecifier = accessSpecifier;
			return result;
		}

		static AstNode* GenerateConversionFunctionIdNode(AstNode* conversionTypeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ConversionFunctionId);
			result->conversionFunctionId.conversionTypeId = conversionTypeId;
			return result;
		}

		static AstNode* GenerateConversionTypeIdNode(AstNode* typeSpecifierSeq, AstNode* conversionDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ConversionTypeId);
			result->conversionTypeId.typeSpecifierSeq = typeSpecifierSeq;
			result->conversionTypeId.conversionDeclarator = conversionDeclarator;
			return result;
		}

		static AstNode* GenerateConversionDeclaratorNode(AstNode* ptrOperator, AstNode* conversionDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ConversionDeclarator);
			result->conversionDeclarator.ptrOperator = ptrOperator;
			result->conversionDeclarator.conversionDeclarator = conversionDeclarator;
			return result;
		}

		static AstNode* GenerateMemInitializerListNode(AstNode* thisMemInitializer, AstNode* nextMemInitializer)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemInitializerList);
			result->memInitializerList.thisMemInitializer = thisMemInitializer;
			result->memInitializerList.nextMemInitializer = nextMemInitializer;
			return result;
		}

		static AstNode* GenerateMemExpressionInitializerNode(AstNode* memInitializerId, AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemExpressionInitializer);
			result->memExpressionInitializer.memInitializerId = memInitializerId;
			result->memExpressionInitializer.expressionList = expressionList;
			return result;
		}

		static AstNode* GenerateMemBracedInitializerNode(AstNode* memInitializerId, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemBracedInitializer);
			result->memBracedInitializer.memInitializerId = memInitializerId;
			result->memBracedInitializer.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GenerateMemInitializerIdNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MemInitializerId);
			result->memInitializerId.identifier = identifier;
			return result;
		}

		static AstNode* GenerateNestedNamespaceSpecifierIdNode(AstNode* nestedNameSpecifier, Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NestedNamespaceSpecifierId);
			result->nestedNamespaceSpecifierId.nestedNameSpecifier = nestedNameSpecifier;
			result->nestedNamespaceSpecifierId.identifier = identifier;
			return result;
		}

		static AstNode* GenerateNestedNamespaceSpecifierTemplateNode(AstNode* nestedNameSpecifier, bool hasTemplateKeyword, AstNode* simpleTemplateId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NestedNamespaceSpecifierTemplate);
			result->nestedNamespaceSpecifierTemplate.nestedNameSpecifier = nestedNameSpecifier;
			result->nestedNamespaceSpecifierTemplate.hasTemplateKeyword = hasTemplateKeyword;
			result->nestedNamespaceSpecifierTemplate.simpleTemplateId = simpleTemplateId;
			return result;
		}

		static AstNode* GeneratePostfixSimpleTypeExpressionListNode(AstNode* simpleTypeSpecifier, AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixSimpleTypeExpressionList);
			result->postfixSimpleTypeExpressionList.simpleTypeSpecifier = simpleTypeSpecifier;
			result->postfixSimpleTypeExpressionList.expressionList = expressionList;
			return result;
		}

		static AstNode* GeneratePostfixSimpleTypeBraceListNode(AstNode* simpleTypeSpecifier, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixSimpleTypeBraceList);
			result->postfixSimpleTypeBraceList.simpleTypeSpecifier = simpleTypeSpecifier;
			result->postfixSimpleTypeBraceList.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GeneratePostfixTypenameSpecExpressionListNode(AstNode* typenameSpecifier, AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixTypenameSpecExpressionList);
			result->postfixTypenameSpecExpressionList.typenameSpecifier = typenameSpecifier;
			result->postfixTypenameSpecExpressionList.expressionList = expressionList;
			return result;
		}

		static AstNode* GeneratePostfixTypenameSpecBraceListNode(AstNode* typenameSpecifier, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixTypenameSpecBraceList);
			result->postfixTypenameSpecBraceList.typenameSpecifier = typenameSpecifier;
			result->postfixTypenameSpecBraceList.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GeneratePostfixCastNode(AstNode* typeId, AstNode* expression, CastType castType)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixCast);
			result->postfixCast.typeId = typeId;
			result->postfixCast.expression = expression;
			result->postfixCast.castType = castType;
			return result;
		}

		static AstNode* GeneratePostfixTypeIdExpressionNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixTypeIdExpression);
			result->postfixTypeIdExpression.expression = expression;
			return result;
		}

		static AstNode* GeneratePostfixTypeIdNode(AstNode* typeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixTypeId);
			result->postfixTypeId.typeId = typeId;
			return result;
		}

		static AstNode* GeneratePostfixBracketExpressionNode(AstNode* postfixExpression, AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixBracketExpression);
			result->postfixBracketExpression.postfixExpression = postfixExpression;
			result->postfixBracketExpression.expression = expression;
			return result;
		}

		static AstNode* GeneratePostfixBracketBraceListNode(AstNode* postfixExpression, AstNode* bracedInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixBracketBraceList);
			result->postfixBracketBraceList.postfixExpression = postfixExpression;
			result->postfixBracketBraceList.bracedInitList = bracedInitList;
			return result;
		}

		static AstNode* GeneratePostfixParenExpressionListNode(AstNode* postfixExpression, AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixParenExpressionList);
			result->postfixParenExpressionList.postfixExpression = postfixExpression;
			result->postfixParenExpressionList.expressionList = expressionList;
			return result;
		}

		static AstNode* GeneratePostfixMemberIdExpressionNode(AstNode* postfixExpression, AstNode* idExpression, bool hasTemplateKeyword, MemberOperatorType memberOp)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixMemberIdExpression);
			result->postfixMemberIdExpression.postfixExpression = postfixExpression;
			result->postfixMemberIdExpression.idExpression = idExpression;
			result->postfixMemberIdExpression.hasTemplateKeyword = hasTemplateKeyword;
			result->postfixMemberIdExpression.memberOp = memberOp;
			return result;
		}

		static AstNode* GeneratePostfixPseudoDestructorNode(AstNode* postfixExpression, AstNode* pseudoDestructorName, MemberOperatorType memberOp)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixPseudoDestructor);
			result->postfixPseudoDestructor.postfixExpression = postfixExpression;
			result->postfixPseudoDestructor.pseudoDestructorName = pseudoDestructorName;
			result->postfixPseudoDestructor.memberOp = memberOp;
			return result;
		}

		static AstNode* GeneratePostfixPlusPlusNode(AstNode* postfixExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixPlusPlus);
			result->postfixPlusPlus.postfixExpression = postfixExpression;
			return result;
		}

		static AstNode* GeneratePostfixMinusMinusNode(AstNode* postfixExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PostfixMinusMinus);
			result->postfixMinusMinus.postfixExpression = postfixExpression;
			return result;
		}

		static AstNode* GeneratePseudoDestructorDecltypeNode(AstNode* decltypeSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PseudoDestructorDecltype);
			result->pseudoDestructorDecltype.decltypeSpecifier = decltypeSpecifier;
			return result;
		}

		static AstNode* GeneratePseudoDestructorTemplateNode(AstNode* nestedNameSpecifier, AstNode* simpleTemplateId, AstNode* typeName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PseudoDestructorTemplate);
			result->pseudoDestructorTemplate.nestedNameSpecifier = nestedNameSpecifier;
			result->pseudoDestructorTemplate.simpleTemplateId = simpleTemplateId;
			result->pseudoDestructorTemplate.typeName = typeName;
			return result;
		}

		static AstNode* GeneratePseudoDestructorNode(AstNode* nestedNameSpecifier, AstNode* typeName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PseudoDestructor);
			result->pseudoDestructor.nestedNameSpecifier = nestedNameSpecifier;
			result->pseudoDestructor.typeName = typeName;
			return result;
		}

		static AstNode* GeneratePseudoNestedDestructorNode(AstNode* nestedNameSpecifier, AstNode* nestedTypeName, AstNode* typeName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PseudoNestedDestructor);
			result->pseudoNestedDestructor.nestedNameSpecifier = nestedNameSpecifier;
			result->pseudoNestedDestructor.nestedTypeName = nestedTypeName;
			result->pseudoNestedDestructor.typeName = typeName;
			return result;
		}

		static AstNode* GenerateNewTypeIdExpressionNode(AstNode* newPlacement, AstNode* newTypeId, AstNode* newInitializer)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewTypeIdExpression);
			result->newTypeIdExpression.newPlacement = newPlacement;
			result->newTypeIdExpression.newTypeId = newTypeId;
			result->newTypeIdExpression.newInitializer = newInitializer;
			return result;
		}

		static AstNode* GenerateNewExpressionNode(AstNode* newPlacement, AstNode* typeId, AstNode* newInitializer)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewExpression);
			result->newExpression.newPlacement = newPlacement;
			result->newExpression.typeId = typeId;
			result->newExpression.newInitializer = newInitializer;
			return result;
		}

		static AstNode* GenerateNewPlacementNode(AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewPlacement);
			result->newPlacementNode.expressionList = expressionList;
			return result;
		}

		static AstNode* GenerateNewTypeIdNode(AstNode* typeSpecifierSeq, AstNode* newDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewTypeId);
			result->newTypeId.typeSpecifierSeq = typeSpecifierSeq;
			result->newTypeId.newDeclarator = newDeclarator;
			return result;
		}

		static AstNode* GenerateNewDeclaratorNode(AstNode* ptrOperator, AstNode* newDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewDeclarator);
			result->newDeclarator.ptrOperator = ptrOperator;
			result->newDeclarator.newDeclarator = newDeclarator;
			return result;
		}

		static AstNode* GenerateNoptrNewTailDeclaratorNode(AstNode* expression, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoptrNewTailDeclarator);
			result->noptrNewTailDeclarator.expression = expression;
			result->noptrNewTailDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateNoptrNewDeclaratorNode(AstNode* noptrNewDeclarator, AstNode* constantExpression, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoptrNewDeclarator);
			result->noptrNewDeclarator.noptrNewDeclarator = noptrNewDeclarator;
			result->noptrNewDeclarator.constantExpression = constantExpression;
			result->noptrNewDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateNewInitializerNode(AstNode* expressionList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NewInitializer);
			result->newInitializer.expressionList = expressionList;
			return result;
		}

		static AstNode* GenerateDeclarationSeqNode(AstNode* thisDeclaration, AstNode* nextDeclaration)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DeclarationSeq);
			result->declarationSeq.thisDeclaration = thisDeclaration;
			result->declarationSeq.nextDeclaration = nextDeclaration;
			return result;
		}

		static AstNode* GenerateAliasDeclarationNode(Token identifier, AstNode* typeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AliasDeclaration);
			result->aliasDeclaration.identifier = identifier;
			result->aliasDeclaration.typeId = typeId;
			return result;
		}

		static AstNode* GenerateSimpleDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* initDeclaratorList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleDeclaration);
			result->simpleDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->simpleDeclaration.declSpecifierSeq = declSpecifierSeq;
			result->simpleDeclaration.initDeclaratorList = initDeclaratorList;
			return result;
		}

		static AstNode* GenerateStaticAssertDeclarationNode(AstNode* constantExpression, Token stringLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::StaticAssertDeclaration);
			result->staticAssertDeclaration.constantExpression = constantExpression;
			result->staticAssertDeclaration.stringLiteral = stringLiteral;
			return result;
		}

		static AstNode* GenerateSimpleDeclSpecifierNode(Token token)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleDeclSpecifier);
			result->simpleDeclSpecifier.token = token;
			return result;
		}

		static AstNode* GenerateDeclSpecifierNode(AstNode* specifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DeclSpecifier);
			result->declSpecifier.specifier = specifier;
			return result;
		}

		static AstNode* GenerateDeclSpecSeqNode(AstNode* thisSpec, AstNode* nextSpec, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DeclSpecSeq);
			result->declSpecSeq.thisSpec = thisSpec;
			result->declSpecSeq.nextSpec = nextSpec;
			result->declSpecSeq.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateStorageClassSpecNode(Token specifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::StorageClassSpec);
			result->storageClassSpec.specifier = specifier;
			return result;
		}

		static AstNode* GenerateFunctionSpecNode(Token specifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionSpec);
			result->functionSpec.specifier = specifier;
			return result;
		}

		static AstNode* GenerateTypedefNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypedefName);
			result->typedefName.identifier = identifier;
			return result;
		}

		static AstNode* GenerateTypeSpecSeqNode(AstNode* thisTypeSpec, AstNode* nextTypeSpec, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeSpecSeq);
			result->typeSpecSeq.thisTypeSpec = thisTypeSpec;
			result->typeSpecSeq.nextTypeSpec = nextTypeSpec;
			result->typeSpecSeq.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateTrailingTypeSpecSeqNode(AstNode* thisTypeSpec, AstNode* nextTypeSpec, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TrailingTypeSpecSeq);
			result->trailingTypeSpecSeq.thisTypeSpec = thisTypeSpec;
			result->trailingTypeSpecSeq.nextTypeSpec = nextTypeSpec;
			result->trailingTypeSpecSeq.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateSimpleTypeTokenSpecNode(Token type)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleTypeTokenSpec);
			result->simpleTypeTokenSpec.type = type;
			return result;
		}

		static AstNode* GenerateSimpleTypeTemplateSpecNode(AstNode* nestedNameSpecifier, AstNode* simpleTemplateId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleTypeTemplateSpec);
			result->simpleTypeTemplateSpec.nestedNameSpec = nestedNameSpecifier;
			result->simpleTypeTemplateSpec.simpleTemplateId = simpleTemplateId;
			return result;
		}

		static AstNode* GenerateSimpleTypeSpecNode(AstNode* nestedNameSpecifier, AstNode* typeName)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleTypeSpec);
			result->simpleTypeSpec.nestedNameSpec = nestedNameSpecifier;
			result->simpleTypeSpec.typeName = typeName;
			return result;
		}

		static AstNode* GenerateDecltypeSpecNode(AstNode* expression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DecltypeSpec);
			result->decltypeSpec.expression = expression;
			return result;
		}

		static AstNode* GenerateAbstractDeclaratorNode(AstNode* noptrAbstractDeclarator, AstNode* parametersAndQualifiers, AstNode* trailingReturnType)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AbstractDeclarator);
			result->abstractDeclarator.noptrAbstractDeclarator = noptrAbstractDeclarator;
			result->abstractDeclarator.parametersAndQualifiers = parametersAndQualifiers;
			result->abstractDeclarator.trailingReturnType = trailingReturnType;
			return result;
		}

		static AstNode* GenerateAbstractElipsisDeclaratorNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::AbstractElipsisDeclarator);
			return result;
		}

		static AstNode* GeneratePtrAbstractDeclaratorNode(AstNode* ptrOperator, AstNode* ptrAbstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PtrAbstractDeclarator);
			result->ptrAbstractDeclarator.ptrOperator = ptrOperator;
			result->ptrAbstractDeclarator.ptrAbstractDeclarator = ptrAbstractDeclarator;
			return result;
		}

		static AstNode* GenerateParameterDeclarationListNode(AstNode* thisParameter, AstNode* nextParameter)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParameterDeclarationList);
			result->parameterDeclarationList.thisParameter = thisParameter;
			result->parameterDeclarationList.nextParameter = nextParameter;
			return result;
		}

		static AstNode* GenerateParameterDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParameterDeclaration);
			result->parameterDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->parameterDeclaration.declSpecifierSeq = declSpecifierSeq;
			result->parameterDeclaration.declarator = declarator;
			return result;
		}

		static AstNode* GenerateParameterDefaultDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator, AstNode* initializerClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParameterDefaultDeclaration);
			result->parameterDefaultDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->parameterDefaultDeclaration.declSpecifierSeq = declSpecifierSeq;
			result->parameterDefaultDeclaration.declarator = declarator;
			result->parameterDefaultDeclaration.initializerClause = initializerClause;
			return result;
		}

		static AstNode* GenerateParameterAbstractDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* abstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParameterAbstractDeclaration);
			result->parameterAbstractDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->parameterAbstractDeclaration.declSpecifierSeq = declSpecifierSeq;
			result->parameterAbstractDeclaration.abstractDeclarator = abstractDeclarator;
			return result;
		}

		static AstNode* GenerateParameterAbstractDefaultDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* abstractDeclarator, AstNode* initializerClause)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ParameterAbstractDefaultDeclaration);
			result->parameterAbstractDefaultDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->parameterAbstractDefaultDeclaration.declSpecifierSeq = declSpecifierSeq;
			result->parameterAbstractDefaultDeclaration.abstractDeclarator = abstractDeclarator;
			result->parameterAbstractDefaultDeclaration.initializerClause = initializerClause;
			return result;
		}

		static AstNode* GenerateFunctionDefaultDefinitionNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator, AutoFunctionType functionType)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionDefaultDefinition);
			result->functionDefaultDefinition.attributeSpecifierSeq = attributeSpecifierSeq;
			result->functionDefaultDefinition.declSpecifierSeq = declSpecifierSeq;
			result->functionDefaultDefinition.declarator = declarator;
			result->functionDefaultDefinition.functionType = functionType;
			return result;
		}

		static AstNode* GenerateFunctionDefinitionNode(AstNode* attributeSpecifierSeq, AstNode* declSpecifierSeq, AstNode* declarator, AstNode* functionBody)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionDefinition);
			result->functionDefinition.attributeSpecifierSeq = attributeSpecifierSeq;
			result->functionDefinition.declSpecifierSeq = declSpecifierSeq;
			result->functionDefinition.declarator = declarator;
			result->functionDefinition.functionBody = functionBody;
			return result;
		}

		static AstNode* GenerateFunctionBodyNode(AstNode* ctorInitializer, AstNode* compoundStatement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionBody);
			result->functionBody.ctorInitializer = ctorInitializer;
			result->functionBody.compoundStatement = compoundStatement;
			return result;
		}

		static AstNode* GenerateLiteralOperatorIdNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LiteralOperatorId);
			result->literalOperatorId.identifier = identifier;
			return result;
		}

		static AstNode* GenerateTemplateDeclarationNode(AstNode* templateParameterList, AstNode* declaration)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TemplateDeclaration);
			result->templateDeclaration.templateParameterList = templateParameterList;
			result->templateDeclaration.declaration = declaration;
			return result;
		}

		static AstNode* GenerateTemplateParameterListNode(AstNode* thisParameter, AstNode* nextParameter)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TemplateParameterList);
			result->templateParameterList.thisParameter = thisParameter;
			result->templateParameterList.nextParameter = nextParameter;
			return result;
		}

		static AstNode* GenerateSimpleTemplateIdNode(AstNode* templateName, AstNode* templateArgumentList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::SimpleTemplateId);
			result->simpleTemplateId.templateName = templateName;
			result->simpleTemplateId.templateArgumentList = templateArgumentList;
			return result;
		}

		static AstNode* GenerateFunctionOperatorTemplateIdNode(AstNode* operatorFunctionId, AstNode* templateArgumentList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionOperatorTemplateId);
			result->functionOperatorTemplateId.operatorFunctionId = operatorFunctionId;
			result->functionOperatorTemplateId.templateArgumentList = templateArgumentList;
			return result;
		}

		static AstNode* GenerateLiteralOperatorTemplateIdNode(AstNode* literalOperatorId, AstNode* templateArgumentList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::LiteralOperatorTemplateId);
			result->literalOperatorTemplateId.literalOperatorId = literalOperatorId;
			result->literalOperatorTemplateId.templateArgumentList = templateArgumentList;
			return result;
		}

		static AstNode* GenerateTemplateNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TemplateName);
			result->templateName.identifier = identifier;
			return result;
		}

		static AstNode* GenerateTypenameSpecifierNode(AstNode* nestedNameSpecifier, Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypenameSpecifier);
			result->typenameSpecifier.nestedNameSpecifier = nestedNameSpecifier;
			result->typenameSpecifier.identifier = identifier;
			return result;
		}

		static AstNode* GenerateTypenameTemplateSpecifierNode(AstNode* nestedNameSpecifier, AstNode* simpleTemplateId, bool hasTemplateKeyword)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypenameTemplateSpecifier);
			result->typenameTemplateSpecifier.nestedNameSpecifier = nestedNameSpecifier;
			result->typenameTemplateSpecifier.simpleTemplateId = simpleTemplateId;
			result->typenameTemplateSpecifier.hasTemplateKeyword = hasTemplateKeyword;
			return result;
		}

		static AstNode* GenerateExplicitInstantiationNode(AstNode* declaration, bool hasExternKeyword)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ExplicitInstantiation);
			result->explicitInstantiation.declaration = declaration;
			result->explicitInstantiation.hasExternKeyword = hasExternKeyword;
			return result;
		}

		static AstNode* GenerateTryBlockNode(AstNode* compoundStatement, AstNode* handlerSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TryBlock);
			result->tryBlock.compoundStatement = compoundStatement;
			result->tryBlock.handlerSeq = handlerSeq;
			return result;
		}

		static AstNode* GenerateFunctionTryBlockNode(AstNode* ctorInitializer, AstNode* compoundStatement, AstNode* handlerSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::FunctionTryBlock);
			result->functionTryBlock.ctorInitializer = ctorInitializer;
			result->functionTryBlock.compoundStatement = compoundStatement;
			result->functionTryBlock.handlerSeq = handlerSeq;
			return result;
		}

		static AstNode* GenerateHandlerSeqNode(AstNode* thisHandler, AstNode* nextHandler)
		{
			AstNode* result = GenerateAstNode(AstNodeType::HandlerSeq);
			result->handlerSeq.thisHandler = thisHandler;
			result->handlerSeq.nextHandler = nextHandler;
			return result;
		}

		static AstNode* GenerateHandlerNode(AstNode* exceptionDeclaration, AstNode* compoundStatement)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Handler);
			result->handler.exceptionDeclaration = exceptionDeclaration;
			result->handler.compoundStatement = compoundStatement;
			return result;
		}

		static AstNode* GenerateExceptionDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* typeSpecifierSeq, AstNode* declarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ExceptionDeclaration);
			result->exceptionDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->exceptionDeclaration.typeSpecifierSeq = typeSpecifierSeq;
			result->exceptionDeclaration.declarator = declarator;
			return result;
		}

		static AstNode* GenerateExceptionAbstractDeclarationNode(AstNode* attributeSpecifierSeq, AstNode* typeSpecifierSeq, AstNode* abstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ExceptionAbstractDeclaration);
			result->exceptionAbstractDeclaration.attributeSpecifierSeq = attributeSpecifierSeq;
			result->exceptionAbstractDeclaration.typeSpecifierSeq = typeSpecifierSeq;
			result->exceptionAbstractDeclaration.abstractDeclarator = abstractDeclarator;
			return result;
		}

		static AstNode* GenerateThrowExpressionNode(AstNode* assignmentExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ThrowExpression);
			result->throwExpression.assignmentExpression = assignmentExpression;
			return result;
		}

		static AstNode* GenerateDynamicExceptionSpecNode(AstNode* typeIdList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::DynamicExceptionSpec);
			result->dynamicExceptionSpec.typeIdList = typeIdList;
			return result;
		}

		static AstNode* GenerateTypeIdListNode(AstNode* thisTypeId, AstNode* nextTypeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeIdList);
			result->typeIdList.thisTypeId = thisTypeId;
			result->typeIdList.nextTypeId = nextTypeId;
			return result;
		}

		static AstNode* GenerateNoexceptExpressionSpecNode(AstNode* constantExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoexceptExpressionSpec);
			result->noexceptExpression.constantExpression = constantExpression;
			return result;
		}

		static AstNode* GenerateNoexceptSpecNode()
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoexceptSpec);
			return result;
		}

		static AstNode* GenerateNoPtrParenDeclaratorNode(AstNode* ptrDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoPtrParenDeclarator);
			result->noPtrParenDeclarator.ptrDeclarator = ptrDeclarator;
			return result;
		}

		static AstNode* GenerateNoPtrBracketDeclaratorNode(AstNode* constantExpression, AstNode* attributeSpecifierSeq, AstNode* noptrDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoPtrBracketDeclarator);
			result->noPtrBracketDeclarator.constantExpression = constantExpression;
			result->noPtrBracketDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			result->noPtrBracketDeclarator.noptrDeclarator = noptrDeclarator;
			return result;
		}

		static AstNode* GenerateNoPtrParamAndQualDeclaratorNode(AstNode* parametersAndQualifiers, AstNode* noptrDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoPtrParamAndQualDeclarator);
			result->noPtrParamAndQualDeclarator.parametersAndQualifiers = parametersAndQualifiers;
			result->noPtrParamAndQualDeclarator.noptrDeclarator = noptrDeclarator;
			return result;
		}

		static AstNode* GenerateNoPtrDeclaratorNode(AstNode* declaratorId, AstNode* attributeSpecifierSeq)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoPtrDeclarator);
			result->noPtrDeclarator.declaratorId = declaratorId;
			result->noPtrDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			return result;
		}

		static AstNode* GenerateBracedInitListNode(AstNode* initializerList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::BracedInitList);
			result->bracedInitList.initializerList = initializerList;
			return result;
		}

		static AstNode* GenerateUSystemDeclarationNode(AstNode* parameterDeclarationClause, AstNode* namedNamespaceDefinition)
		{
			AstNode* result = GenerateAstNode(AstNodeType::USystemDeclaration);
			result->uSystemDeclaration.parameterDeclarationClause = parameterDeclarationClause;
			result->uSystemDeclaration.namedNamespaceDefinition = namedNamespaceDefinition;
			return result;
		}

		static AstNode* GenerateInitializerListNode(AstNode* thisInitList, AstNode* nextInitList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::InitializerList);
			result->initializerList.thisInitList = thisInitList;
			result->initializerList.nextInitList = nextInitList;
			return result;
		}

		static AstNode* GenerateTypeTemplateParameterNode(AstNode* templateParameterList, Token identifier, AstNode* idExpression)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeTemplateParameter);
			result->typeTemplateParameter.templateParameterList = templateParameterList;
			result->typeTemplateParameter.identifier = identifier;
			result->typeTemplateParameter.idExpression = idExpression;
			return result;
		}

		static AstNode* GenerateTypeTypenameParameterNode(Token identifier, AstNode* typeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeTypenameParameter);
			result->typeTypenameParameter.identifier = identifier;
			result->typeTypenameParameter.typeId = typeId;
			return result;
		}

		static AstNode* GenerateTypeClassParameterNode(Token identifier, AstNode* typeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TypeClassParameter);
			result->typeClassParameter.identifier = identifier;
			result->typeClassParameter.typeId = typeId;
			return result;
		}

		static AstNode* GenerateNoptrAbstractDeclaratorNode(AstNode* ptrAbstractDeclarator, AstNode* parametersAndQualifiers, AstNode* noptrAbstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoptrAbstractDeclarator);
			result->noptrAbstractDeclarator.ptrAbstractDeclarator = ptrAbstractDeclarator;
			result->noptrAbstractDeclarator.parametersAndQualifiers = parametersAndQualifiers;
			result->noptrAbstractDeclarator.noptrAbstractDeclarator = noptrAbstractDeclarator;
			return result;
		}

		static AstNode* GenerateNoptrAbstractExpressionDeclaratorNode(AstNode* ptrAbstractDeclarator, AstNode* constantExpression, AstNode* attributeSpecifierSeq, AstNode* noptrAbstractDeclarator)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NoptrAbstractExpressionDeclarator);
			result->noptrAbstractExpressionDeclarator.ptrAbstractDeclarator = ptrAbstractDeclarator;
			result->noptrAbstractExpressionDeclarator.constantExpression = constantExpression;
			result->noptrAbstractExpressionDeclarator.attributeSpecifierSeq = attributeSpecifierSeq;
			result->noptrAbstractExpressionDeclarator.noptrAbstractDeclarator = noptrAbstractDeclarator;
			return result;
		}

		static AstNode* GenerateUnqualifiedIdNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UnqualifiedId);
			result->unqualifiedId.identifier = identifier;
			return result;
		}

		static AstNode* GenerateUnqualifiedIdDtorClassNode(AstNode* className)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UnqualifiedIdDtorClass);
			result->unqualifiedIdDtorClass.className = className;
			return result;
		}

		static AstNode* GenerateUnqualifiedIdDtorDecltypeNode(AstNode* decltypeSpecifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::UnqualifiedIdDtorDecltype);
			result->unqualifiedIdDtorDecltype.decltypeSpecifier = decltypeSpecifier;
			return result;
		}

		static AstNode* GenerateElaboratedSpecifierEnumNode(AstNode* nestedNameSpecifier, Token identifier, bool hasScopeOp)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElaboratedSpecifierEnum);
			result->elaboratedSpecifierEnum.nestedNameSpecifier = nestedNameSpecifier;
			result->elaboratedSpecifierEnum.identifier = identifier;
			result->elaboratedSpecifierEnum.hasScopeOp = hasScopeOp;
			return result;
		}

		static AstNode* GenerateElaboratedSpecifierTemplateNode(AstNode* classKey, AstNode* nestedNameSpecifier, AstNode* simpleTemplateId, bool hasScopeOp, bool hasTemplateKeyword)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElaboratedSpecifierTemplate);
			result->elaboratedSpecifierTemplate.classKey = classKey;
			result->elaboratedSpecifierTemplate.nestedNameSpecifier = nestedNameSpecifier;
			result->elaboratedSpecifierTemplate.simpleTemplateId = simpleTemplateId;
			result->elaboratedSpecifierTemplate.hasScopeOp = hasScopeOp;
			result->elaboratedSpecifierTemplate.hasTemplateKeyword = hasTemplateKeyword;
			return result;
		}

		static AstNode* GenerateElaboratedSpecifierClassNode(AstNode* classKey, AstNode* attributeSpecifierSeq, AstNode* nestedNameSpecifier, Token identifier, bool hasScopeOp)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElaboratedSpecifierClass);
			result->elaboratedSpecifierClass.classKey = classKey;
			result->elaboratedSpecifierClass.attributeSpecifierSeq = attributeSpecifierSeq;
			result->elaboratedSpecifierClass.nestedNameSpecifier = nestedNameSpecifier;
			result->elaboratedSpecifierClass.identifier = identifier;
			result->elaboratedSpecifierClass.hasScopeOp = hasScopeOp;
			return result;
		}

		static AstNode* GenerateAlignmentExpressionNode(AstNode* typeId)
		{
			AstNode* result = GenerateAstNode(AstNodeType::AlignmentExpression);
			result->alignmentExpression.typeId = typeId;
			return result;
		}

		static AstNode* GenerateNoSuccessAstNode()
		{
			AstNode* node = GenerateAstNode(AstNodeType::None);
			node->success = false;
			return node;
		}

		static PreprocessingAstNode* GenerateNoSuccessPreprocessingAstNode()
		{
			PreprocessingAstNode* node = GeneratePreprocessingAstNode(PreprocessingAstNodeType::None);
			node->success = false;
			return node;
		}

		static PreprocessingAstNode* GeneratePreprocessingFileNode(PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::PreprocessingFile);
			result->preprocessingFile.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateGroupNode(PreprocessingAstNode* thisGroupPart, PreprocessingAstNode* nextGroupPart)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::Group);
			result->group.thisGroupPart = thisGroupPart;
			result->group.nextGroupPart = nextGroupPart;
			return result;
		}

		static PreprocessingAstNode* GenerateIfSectionNode(PreprocessingAstNode* ifGroup, PreprocessingAstNode* elifGroups, PreprocessingAstNode* elseGroup)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::IfSection);
			result->ifSection.ifGroup = ifGroup;
			result->ifSection.elifGroups = elifGroups;
			result->ifSection.elseGroup = elseGroup;
			return result;
		}

		static PreprocessingAstNode* GenerateIfGroupNode(AstNode* constantExpression, PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::IfGroup);
			result->ifGroup.constantExpression = constantExpression;
			result->ifGroup.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateIfDefGroupNode(Token identifier, PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::IfDefGroup);
			result->ifDefGroup.identifier = identifier;
			result->ifDefGroup.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateIfNDefGroupNode(Token identifier, PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::IfNDefGroup);
			result->ifNDefGroup.identifier = identifier;
			result->ifNDefGroup.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateElifGroupsNode(PreprocessingAstNode* thisElifGroup, PreprocessingAstNode* nextElifGroup)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::ElifGroups);
			result->elifGroups.thisElifGroup = thisElifGroup;
			result->elifGroups.nextElifGroup = nextElifGroup;
			return result;
		}

		static PreprocessingAstNode* GenerateElifGroupNode(AstNode* constantExpression, PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::ElifGroup);
			result->elifGroup.constantExpression = constantExpression;
			result->elifGroup.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateElseGroupNode(PreprocessingAstNode* group)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::ElseGroup);
			result->elseGroup.group = group;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroIncludeNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroInclude);
			result->macroInclude.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroDefineNode(Token identifier, PreprocessingAstNode* replacementList)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroDefine);
			result->macroDefine.identifier = identifier;
			result->macroDefine.replacementList = replacementList;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroDefineFunctionNode(Token identifier, PreprocessingAstNode* identifierList, PreprocessingAstNode* replacementList)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroDefineFunction);
			result->macroDefineFunction.identifier = identifier;
			result->macroDefineFunction.identifierList = identifierList;
			result->macroDefineFunction.replacementList = replacementList;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroUndefNode(Token identifier)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroUndef);
			result->macroUndef.identifier = identifier;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroLineNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroLine);
			result->macroLine.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroErrorNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroError);
			result->macroError.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateMacroPragmaNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::MacroPragma);
			result->macroPragma.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateTextLineNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::TextLine);
			result->textLine.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateNonDirectiveNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::NonDirective);
			result->nonDirective.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GenerateIdentifierListNode(PreprocessingAstNode* thisIdentifierNode, PreprocessingAstNode* nextIdentifierNode)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::IdentifierList);
			result->identifierList.thisIdentifierNode = thisIdentifierNode;
			result->identifierList.nextIdentifierNode = nextIdentifierNode;
			return result;
		}

		static PreprocessingAstNode* GenerateIdentifierNode(Token identifier)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::Identifier);
			result->identifier.identifier = identifier;
			return result;
		}

		static PreprocessingAstNode* GenerateReplacementListNode(PreprocessingAstNode* ppTokens)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::ReplacementList);
			result->replacementList.ppTokens = ppTokens;
			return result;
		}

		static PreprocessingAstNode* GeneratePPTokensNode(PreprocessingAstNode* preprocessingToken, PreprocessingAstNode* nextPreprocessingToken)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::PPTokens);
			result->ppTokens.preprocessingToken = preprocessingToken;
			result->ppTokens.nextPreprocessingToken = nextPreprocessingToken;
			return result;
		}

		static PreprocessingAstNode* GenerateEmptyMacroNode()
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::EmptyMacro);
			return result;
		}

		static PreprocessingAstNode* GenerateStringLiteralNode(Token stringLiteral)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::StringLiteral);
			result->stringLiteral.stringLiteral = stringLiteral;
			return result;
		}

		static PreprocessingAstNode* GenerateNumberLiteralNode(Token numberLiteral)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::NumberLiteral);
			result->numberLiteral.numberLiteral = numberLiteral;
			return result;
		}

		static PreprocessingAstNode* GenerateCharacterLiteralNode(Token characterLiteral)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::CharacterLiteral);
			result->characterLiteral.characterLiteral = characterLiteral;
			return result;
		}

		static PreprocessingAstNode* GenerateHeaderNameNode(Token identifier)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::HeaderName);
			result->headerName.identifier = identifier;
			return result;
		}

		static PreprocessingAstNode* GenerateHeaderNameStringNode(Token stringLiteral)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::HeaderNameString);
			result->headerNameString.stringLiteral = stringLiteral;
			return result;
		}

		static PreprocessingAstNode* GeneratePreprocessingOpOrPuncNode(Token opOrPunc)
		{
			PreprocessingAstNode* result = GeneratePreprocessingAstNode(PreprocessingAstNodeType::PreprocessingOpOrPunc);
			result->preprocessingOpOrPunc.opOrPunc = opOrPunc;
			return result;
		}

		// ===============================================================================================
		// Parser Forward Declarations (internal)
		// ===============================================================================================
		// Translation Unit
		static AstNode* ParseTranslationUnit(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, ParserData& data);

		// Expressions
		static AstNode* ParsePrimaryExpression(ParserData& data);
		static AstNode* ParseIdExpression(ParserData& data);
		static AstNode* ParseUnqualifiedId(ParserData& data);
		static AstNode* ParseQualifiedId(ParserData& data);
		static AstNode* ParseNestedNameSpecifier(ParserData& data);

		// Lambdas
		static AstNode* ParseLambdaExpression(ParserData& data);
		static AstNode* ParseLambdaIntroducer(ParserData& data);
		static AstNode* ParseLambdaCapture(ParserData& data);
		static AstNode* ParseCaptureList(ParserData& data);
		static AstNode* ParseCapture(ParserData& data);
		static AstNode* ParseLambdaDeclarator(ParserData& data);

		// Postfix Expressions
		static AstNode* ParsePostfixExpression(ParserData& data);
		static AstNode* ParseExpressionList(ParserData& data);
		static AstNode* ParsePseudoDestructorName(ParserData& data);

		// Unary Expressions
		static AstNode* ParseUnaryExpression(ParserData& data);

		// New Expressions
		static AstNode* ParseNewExpression(ParserData& data);
		static AstNode* ParseNewPlacement(ParserData& data);
		static AstNode* ParseNewTypeId(ParserData& data);
		static AstNode* ParseNewDeclarator(ParserData& data);
		static AstNode* ParseNoptrNewDeclarator(ParserData& data);
		static AstNode* ParseNewInitializer(ParserData& data);

		// Delete
		static AstNode* ParseDeleteExpression(ParserData& data);

		// Noexcept
		static AstNode* ParseNoexceptExpression(ParserData& data);

		// Cast
		static AstNode* ParseCastExpression(ParserData& data);

		// Pointer to member
		static AstNode* ParsePmExpression(ParserData& data);

		// Primary operations
		static AstNode* ParseMultiplicativeExpression(ParserData& data);
		static AstNode* ParseAdditiveExpression(ParserData& data);
		static AstNode* ParseShiftExpression(ParserData& data);

		// Comparision operations
		static AstNode* ParseRelationalExpression(ParserData& data);
		static AstNode* ParseEqualityExpression(ParserData& data);
		static AstNode* ParseAndExpression(ParserData& data);

		// Logical operations
		static AstNode* ParseExclusiveOrExpression(ParserData& data);
		static AstNode* ParseInclusiveOrExpression(ParserData& data);
		static AstNode* ParseLogicalAndExpression(ParserData& data);
		static AstNode* ParseLogicalOrExpression(ParserData& data);

		// Misc expressions
		static AstNode* ParseConditionalExpression(ParserData& data);
		static AstNode* ParseAssignmentExpression(ParserData& data);
		static AstNode* ParseAlignmentExpression(ParserData& data);

		static AstNode* ParseExpression(ParserData& data);
		static AstNode* ParseConstantExpression(ParserData& data);

		// Statements
		static AstNode* ParseStatement(ParserData& data);
		static AstNode* ParseLabeledStatement(ParserData& data);
		static AstNode* ParseExpressionStatement(ParserData& data);
		static AstNode* ParseCompoundStatement(ParserData& data);
		static AstNode* ParseStatementSequence(ParserData& data);

		// Selection statements
		static AstNode* ParseSelectionStatement(ParserData& data);
		static AstNode* ParseCondition(ParserData& data);

		// Iteration statements
		static AstNode* ParseIterationStatement(ParserData& data);
		static AstNode* ParseForInitStatement(ParserData& data);
		static AstNode* ParseForRangeDeclaration(ParserData& data);
		static AstNode* ParseForRangeInitializer(ParserData& data);

		// Jump statements
		static AstNode* ParseJumpStatement(ParserData& data);

		// Declarations
		static AstNode* ParseDeclarationStatement(ParserData& data);
		static AstNode* ParseDeclarationSequence(ParserData& data);
		static AstNode* ParseUSystemDeclaration(ParserData& data);
		static AstNode* ParseDeclaration(ParserData& data);
		static AstNode* ParseBlockDeclaration(ParserData& data);
		static AstNode* ParseAliasDeclaration(ParserData& data);
		static AstNode* ParseSimpleDeclaration(ParserData& data);
		static AstNode* ParseStaticAssertDeclaration(ParserData& data);
		static AstNode* ParseEmptyDeclaration(ParserData& data);
		static AstNode* ParseAttributeDeclaration(ParserData& data);

		static AstNode* ParseDeclarationSpecifier(ParserData& data);
		static AstNode* ParseDeclarationSpecifierSequence(ParserData& data);
		static AstNode* ParseStorageClassSpecifier(ParserData& data);
		static AstNode* ParseFunctionSpecifier(ParserData& data);

		// Types/typedefs
		static AstNode* ParseTypedefName(ParserData& data);
		static AstNode* ParseTypeSpecifier(ParserData& data);
		static AstNode* ParseTrailingTypeSpecifier(ParserData& data);
		static AstNode* ParseTypeSpecifierSequence(ParserData& data);
		static AstNode* ParseTrailingTypeSpecifierSequence(ParserData& data);

		static AstNode* ParseSimpleTypeSpecifier(ParserData& data);
		static AstNode* ParseTypeName(ParserData& data);
		static AstNode* ParseDecltypeSpecifier(ParserData& data);
		static AstNode* ParseElaboratedTypeSpecifier(ParserData& data);

		// Enums
		static AstNode* ParseEnumName(ParserData& data);
		static AstNode* ParseEnumSpecifier(ParserData& data);
		static AstNode* ParseEnumHead(ParserData& data);
		static AstNode* ParseOpaqueEnumDeclaration(ParserData& data);
		static AstNode* ParseEnumKey(ParserData& data);
		static AstNode* ParseEnumBase(ParserData& data);
		static AstNode* ParseEnumeratorList(ParserData& data);
		static AstNode* ParseEnumeratorDefinition(ParserData& data);

		// Namespaces
		static AstNode* ParseNamespaceName(ParserData& data);
		static AstNode* ParseNamespaceDefinition(ParserData& data);
		static AstNode* ParseNamedNamespaceDefinition(ParserData& data);
		static AstNode* ParseUnnamedNamespaceDefinition(ParserData& data);
		static AstNode* ParseNamespaceBody(ParserData& data);

		// Namespace alias
		static AstNode* ParseNamespaceAliasDefinition(ParserData& data);
		static AstNode* ParseQualifiedNamespaceSpecifier(ParserData& data);

		// Using
		static AstNode* ParseUsingDeclaration(ParserData& data);
		static AstNode* ParseUsingDirective(ParserData& data);
		static AstNode* ParseAsmDefinition(ParserData& data);
		static AstNode* ParseLinkageSpecification(ParserData& data);

		// Declaration Grammar
		static AstNode* ParseAttributeSpecifierSequence(ParserData& data);
		static AstNode* ParseAttributeSpecifier(ParserData& data);
		static AstNode* ParseAlignmentSpecifier(ParserData& data);
		static AstNode* ParseAttributeList(ParserData& data);
		static AstNode* ParseAttribute(ParserData& data);
		static AstNode* ParseAttributeToken(ParserData& data);
		static AstNode* ParseAttributeArgumentClause(ParserData& data);
		static AstNode* ParseBalancedTokenSequence(ParserData& data);
		static AstNode* ParseBalancedToken(ParserData& data);

		// Declarations
		static AstNode* ParseInitDeclaratorList(ParserData& data);
		static AstNode* ParseInitDeclarator(ParserData& data);
		static AstNode* ParseDeclarator(ParserData& data);
		static AstNode* ParsePtrDeclarator(ParserData& data);
		static AstNode* ParseNoPtrDeclarator(ParserData& data);
		static AstNode* ParseParametersAndQualifiers(ParserData& data);
		static AstNode* ParseTrailingReturnType(ParserData& data);
		static AstNode* ParsePtrOperator(ParserData& data);
		static AstNode* ParseCvQualifierSequence(ParserData& data);
		static AstNode* ParseCvQualifier(ParserData& data);
		static AstNode* ParseRefQualifier(ParserData& data);
		static AstNode* ParseDeclaratorId(ParserData& data);

		// dcl.name
		static AstNode* ParseTypeId(ParserData& data);
		static AstNode* ParseAbstractDeclarator(ParserData& data);
		static AstNode* ParsePtrAbstractDeclarator(ParserData& data);
		static AstNode* ParseNoptrAbstractDeclarator(ParserData& data);

		// dcl.fct
		static AstNode* ParseParameterDeclarationClause(ParserData& data);
		static AstNode* ParseParameterDeclarationList(ParserData& data);
		static AstNode* ParseParameterDeclaration(ParserData& data);

		// Functions
		static AstNode* ParseFunctionDefinition(ParserData& data);
		static AstNode* ParseFunctionBody(ParserData& data);

		// Init
		static AstNode* ParseInitializer(ParserData& data);
		static AstNode* ParseBraceOrEqualInitializer(ParserData& data);
		static AstNode* ParseInitializerClause(ParserData& data);
		static AstNode* ParseInitializerList(ParserData& data);
		static AstNode* ParseBracedInitList(ParserData& data);

		// Classes
		static AstNode* ParseClassName(ParserData& data);
		static AstNode* ParseClassSpecifier(ParserData& data);
		static AstNode* ParseClassHead(ParserData& data);
		static AstNode* ParseClassHeadName(ParserData& data);
		static AstNode* ParseClassVirtSpecifierSequence(ParserData& data);
		static AstNode* ParseClassVirtSpecifier(ParserData& data);
		static AstNode* ParseClassKey(ParserData& data);

		// Class Members
		static AstNode* ParseMemberSpecification(ParserData& data);
		static AstNode* ParseMemberDeclaration(ParserData& data);
		static AstNode* ParseMemberDeclaratorList(ParserData& data);
		static AstNode* ParseMemberDeclarator(ParserData& data);
		static AstNode* ParseVirtSpecifierSequence(ParserData& data);
		static AstNode* ParseVirtSpecifier(ParserData& data);
		static AstNode* ParsePureSpecifier(ParserData& data);

		// Derived classes
		static AstNode* ParseBaseClause(ParserData& data);
		static AstNode* ParseBaseSpecifierList(ParserData& data);
		static AstNode* ParseBaseSpecifier(ParserData& data);
		static AstNode* ParseClassOrDecltype(ParserData& data);
		static AstNode* ParseBaseTypeSpecifier(ParserData& data);
		static AstNode* ParseAccessSpecifier(ParserData& data);

		// Class conversion functions
		static AstNode* ParseConversionFunctionId(ParserData& data);
		static AstNode* ParseConversionTypeId(ParserData& data);
		static AstNode* ParseConversionDeclarator(ParserData& data);

		// Class initializers
		static AstNode* ParseCtorInitializer(ParserData& data);
		static AstNode* ParseMemInitializerList(ParserData& data);
		static AstNode* ParseMemInitializer(ParserData& data);
		static AstNode* ParseMemInitializerId(ParserData& data);

		// Operator overloading
		static AstNode* ParseOperatorFunctionId(ParserData& data);
		static OverloadableOperatorType ParseOverloadableOperator(ParserData& data);

		// Literal overrides
		static AstNode* ParseLiteralOperatorId(ParserData& data);

		// Templates
		static AstNode* ParseTemplateDeclaration(ParserData& data);
		static AstNode* ParseTemplateParameterList(ParserData& data);
		static AstNode* ParseTemplateParameter(ParserData& data);
		static AstNode* ParseTypeParameter(ParserData& data);
		static AstNode* ParseSimpleTemplateId(ParserData& data);
		static AstNode* ParseTemplateId(ParserData& data);
		static AstNode* ParseTemplateName(ParserData& data);
		static AstNode* ParseTemplateArgumentList(ParserData& data);
		static AstNode* ParseTemplateArgument(ParserData& data);

		static AstNode* ParseTypenameSpecifier(ParserData& data);
		static AstNode* ParseExplicitInstantiation(ParserData& data);
		static AstNode* ParseExplicitSpecialization(ParserData& data);

		// Exceptions
		static AstNode* ParseTryBlock(ParserData& data);
		static AstNode* ParseFunctionTryBlock(ParserData& data);
		static AstNode* ParseHandlerSequence(ParserData& data);
		static AstNode* ParseHandler(ParserData& data);
		static AstNode* ParseExceptionDeclaration(ParserData& data);

		static AstNode* ParseThrowExpression(ParserData& data);
		static AstNode* ParseExceptionSpecification(ParserData& data);
		static AstNode* ParseDynamicExceptionSpecification(ParserData& data);
		static AstNode* ParseTypeIdList(ParserData& data);
		static AstNode* ParseNoexceptSpecification(ParserData& data);

		// ===============================================================================================
		// Parser Preprocessor Forward Declarations (internal)
		// ===============================================================================================
		static void Preprocess(const char* fileBeingParsed, const std::vector<std::filesystem::path>& includeDirs, ParserData& data);

		// Preprocessor File
		static PreprocessingAstNode* ParsePreprocessingFile(ParserData& data);
		static PreprocessingAstNode* ParseGroup(ParserData& data);
		static PreprocessingAstNode* ParseGroupPart(ParserData& data);
		static PreprocessingAstNode* ParseIfSection(ParserData& data);
		static PreprocessingAstNode* ParseIfGroup(ParserData& data);
		static PreprocessingAstNode* ParseElifGroups(ParserData& data);
		static PreprocessingAstNode* ParseElifGroup(ParserData& data);
		static PreprocessingAstNode* ParseElseGroup(ParserData& data);
		static PreprocessingAstNode* ParseMacroInclude(ParserData& data);
		static PreprocessingAstNode* ParseControlLine(ParserData& data);
		static PreprocessingAstNode* ParseTextLine(ParserData& data);
		static PreprocessingAstNode* ParseNonDirective(ParserData& data);
		static PreprocessingAstNode* ParseIdentifierList(ParserData& data);
		static PreprocessingAstNode* ParseReplacementList(ParserData& data);
		static PreprocessingAstNode* ParsePPTokens(ParserData& data, bool isHeader = false);
		static PreprocessingAstNode* ParseNumberLiteral(ParserData& data);

		// Preprocessor Stuff
		static PreprocessingAstNode* ParsePreprocessingToken(ParserData& data, bool isHeader = false);
		static PreprocessingAstNode* ParseHeaderName(ParserData& data);
		static PreprocessingAstNode* ParseCharacterLiteral(ParserData& data);
		static PreprocessingAstNode* ParseUserDefinedCharacterLiteral(ParserData& data);
		static PreprocessingAstNode* ParseStringLiteral(ParserData& data);
		static PreprocessingAstNode* ParseUserDefinedStringLiteral(ParserData& data);
		static PreprocessingAstNode* ParsePreprocessingOpOrPunc(ParserData& data);
		static PreprocessingAstNode* ParseHCharSequence(ParserData& data);
		static PreprocessingAstNode* ParseHChar(ParserData& data);
		static PreprocessingAstNode* ParseQCharSequence(ParserData& data);
		static PreprocessingAstNode* ParseQChar(ParserData& data);

		// ===============================================================================================
		// Parser Implementation (internal)
		//
		// These implement the grammar rules found at: https://www.nongnu.org/hcb/#decl-specifier-seq
		// It has been modified where needed.
		// ===============================================================================================
		// Translation Unit
		static AstNode* ParseTranslationUnit(const char* fileBeingParsed, std::vector<std::filesystem::path>& includeDirs, ParserData& data)
		{
			data.Tokens.insert(CppTokens::CreateToken(-1, -1, TokenType::PREPROCESSING_FILE_BEGIN, fileBeingParsed), 0);
			data.Tokens.push(CppTokens::CreateToken(-1, -1, TokenType::PREPROCESSING_FILE_END, fileBeingParsed));
			Preprocess(fileBeingParsed, includeDirs, data);
			data.CurrentToken = 0;
			return ParseDeclarationSequence(data);
		}

		// Expressions
		static AstNode* ParsePrimaryExpression(ParserData& data)
		{
			if (PeekIn(data, { TokenType::CHARACTER_LITERAL, TokenType::FLOATING_POINT_LITERAL, TokenType::INTEGER_LITERAL, TokenType::STRING_LITERAL }))
			{
				return GenerateLiteralNode(ConsumeCurrent(data, Peek(data)));
			}

			if (Peek(data) == TokenType::KW_THIS)
			{
				return GenerateThisNode(ConsumeCurrent(data, Peek(data)));
			}

			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GenerateGroupingNode(expression);
			}

			int backtrackPosition = data.CurrentToken;
			AstNode* expr = ParseIdExpression(data);
			if (expr->success)
			{
				return expr;
			}
			FreeNode(expr);
			BacktrackTo(data, backtrackPosition);

			AstNode* lambdaExpr = ParseLambdaExpression(data);
			if (lambdaExpr->success)
			{
				return lambdaExpr;
			}
			FreeNode(lambdaExpr);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseIdExpression(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* unqualifiedId = ParseUnqualifiedId(data);
			if (unqualifiedId->success)
			{
				return unqualifiedId;
			}
			FreeNode(unqualifiedId);
			BacktrackTo(data, backtrackPosition);

			AstNode* qualifiedId = ParseQualifiedId(data);
			if (qualifiedId->success)
			{
				return qualifiedId;
			}
			FreeNode(qualifiedId);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUnqualifiedId(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateUnqualifiedIdNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::TILDE))
			{
				AstNode* className = ParseClassName(data);
				if (className->success)
				{
					return GenerateUnqualifiedIdDtorClassNode(className);
				}
				FreeNode(className);
				BacktrackTo(data, backtrackPosition);

				Consume(data, TokenType::TILDE);
				AstNode* decltypeSpecifier = ParseDecltypeSpecifier(data);
				if (decltypeSpecifier->success)
				{
					return GenerateUnqualifiedIdDtorDecltypeNode(decltypeSpecifier);
				}
				FreeNode(decltypeSpecifier);
			}
			BacktrackTo(data, backtrackPosition);

			AstNode* operatorFunctionId = ParseOperatorFunctionId(data);
			if (operatorFunctionId->success)
			{
				return operatorFunctionId;
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(data, backtrackPosition);

			AstNode* conversionFunctionId = ParseConversionFunctionId(data);
			if (conversionFunctionId->success)
			{
				return conversionFunctionId;
			}
			FreeNode(conversionFunctionId);
			BacktrackTo(data, backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId(data);
			if (literalOperatorId->success)
			{
				return literalOperatorId;
			}
			FreeNode(literalOperatorId);
			BacktrackTo(data, backtrackPosition);

			AstNode* templateId = ParseTemplateId(data);
			if (templateId->success)
			{
				return templateId;
			}
			FreeNode(templateId);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseQualifiedId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			bool hasNamespaceScope = Peek(data) == TokenType::COLON;
			if (Peek(data) == TokenType::COLON)
			{
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			bool hasTemplateKeyword = Match(data, TokenType::KW_TEMPLATE);
			AstNode* unqualifiedId = ParseUnqualifiedId(data);
			if (!hasNamespaceScope)
			{
				// This can only be a nested-namespace-specifier if it doesn't have a namespace scope
				if (nestedNameSpecifier->success && unqualifiedId->success)
				{
					return GenerateTemplateQualifiedIdNode(nestedNameSpecifier, hasNamespaceScope, hasTemplateKeyword);
				}
				FreeNode(nestedNameSpecifier);
				FreeNode(unqualifiedId);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (nestedNameSpecifier->success && unqualifiedId->success)
			{
				return GenerateTemplateQualifiedIdNode(nestedNameSpecifier, hasNamespaceScope, hasTemplateKeyword);
			}
			FreeNode(nestedNameSpecifier);
			FreeNode(unqualifiedId);
			BacktrackTo(data, backtrackPosition);

			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateQualifiedIdNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			AstNode* operatorFunctionId = ParseOperatorFunctionId(data);
			if (operatorFunctionId->success)
			{
				return operatorFunctionId;
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(data, backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId(data);
			if (literalOperatorId->success)
			{
				return literalOperatorId;
			}
			FreeNode(literalOperatorId);
			BacktrackTo(data, backtrackPosition);

			AstNode* templateId = ParseTemplateId(data);
			if (templateId->success)
			{
				return templateId;
			}
			FreeNode(templateId);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNestedNameSubSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* typeName = ParseTypeName(data);
			if (typeName->success)
			{
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
					return typeName;
				}
			}
			FreeNode(typeName);
			BacktrackTo(data, backtrackPosition);

			AstNode* namespaceName = ParseNamespaceName(data);
			if (namespaceName->success)
			{
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
					return namespaceName;
				}
			}
			FreeNode(namespaceName);
			BacktrackTo(data, backtrackPosition);

			AstNode* decltypeSpecifier = ParseDecltypeSpecifier(data);
			if (decltypeSpecifier->success)
			{
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
					return decltypeSpecifier;
				}
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNestedNameSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* nestedNameSubSpecifier = ParseNestedNameSubSpecifier(data);
			if (nestedNameSubSpecifier->success)
			{
				return nestedNameSubSpecifier;
			}
			FreeNode(nestedNameSubSpecifier);
			BacktrackTo(data, backtrackPosition);

			// If the next token is not an identifier, or a template OR there are no more semicolons, then return no success
			if (!PeekIn(data, { TokenType::IDENTIFIER, TokenType::KW_TEMPLATE }) || !MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				return GenerateNoSuccessAstNode();
			}
			AstNode* nestedNameSpecifier = ParseNestedNameSpecifier(data);
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				Token id = ConsumeCurrent(data, TokenType::IDENTIFIER);
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::COLON);
				return GenerateNestedNamespaceSpecifierIdNode(nestedNameSpecifier, id);
			}

			bool hasTemplateKeyword = Match(data, TokenType::KW_TEMPLATE);
			AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
			if (simpleTemplateId->success)
			{
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::COLON);
				return GenerateNestedNamespaceSpecifierTemplateNode(nestedNameSpecifier, hasTemplateKeyword, simpleTemplateId);
			}
			FreeNode(nestedNameSpecifier);
			FreeNode(simpleTemplateId);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Lambdas
		static AstNode* ParseLambdaExpression(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* lambdaIntroducer = ParseLambdaIntroducer(data);
			if (!lambdaIntroducer->success)
			{
				FreeNode(lambdaIntroducer);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// This is optional, so it's fine if it doesn't succeed
			backtrackPosition = data.CurrentToken;
			AstNode* lambdaDeclarator = ParseLambdaDeclarator(data);
			if (!lambdaDeclarator->success)
			{
				BacktrackTo(data, backtrackPosition);
			}

			backtrackPosition = data.CurrentToken;
			AstNode* compoundStatement = ParseCompoundStatement(data);
			if (!compoundStatement->success)
			{
				FreeNode(lambdaIntroducer);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateLambdaExpressionNode(lambdaIntroducer, lambdaDeclarator, compoundStatement);
		}

		static AstNode* ParseLambdaIntroducer(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_BRACKET))
			{
				int backtrackPosition = data.CurrentToken;
				// Lambda capture is optional, so it's ok if it fails
				AstNode* lambdaCapture = ParseLambdaCapture(data);
				if (!lambdaCapture->success)
				{
					BacktrackTo(data, backtrackPosition);
				}
				Consume(data, TokenType::RIGHT_BRACKET);

				return GenerateLambdaIntroducerNode(lambdaCapture);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLambdaCapture(ParserData& data)
		{
			Token captureDefault;
			captureDefault.m_Type = TokenType::None;
			if (Peek(data) == TokenType::AND || Peek(data) == TokenType::EQUAL)
			{
				captureDefault = ConsumeCurrent(data, Peek(data));
			}

			if (captureDefault.m_Type != TokenType::None && Match(data, TokenType::COMMA))
			{
				return GenerateLambdaCaptureNode(captureDefault, ParseCaptureList(data));
			}

			if (captureDefault.m_Type == TokenType::None)
			{
				return GenerateLambdaCaptureNode(captureDefault, ParseCaptureList(data));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCaptureList(ParserData& data)
		{
			AstNode* result = ParseCapture(data);

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateLambdaCaptureListNode(result, ParseCaptureList(data));
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return GenerateLambdaCaptureListNode(result, nullptr);
		}

		static AstNode* ParseCapture(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::AND))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					return GenerateCaptureNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
				}

				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Peek(data) == TokenType::KW_THIS)
			{
				return GenerateThisNode(ConsumeCurrent(data, TokenType::KW_THIS));
			}

			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateCaptureNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLambdaDeclarator(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause(data);
				Consume(data, TokenType::RIGHT_PAREN);

				bool isMutable = Match(data, TokenType::KW_MUTABLE);

				int backtrackPosition = data.CurrentToken;
				AstNode* exceptionSpec = ParseExceptionSpecification(data);
				if (!exceptionSpec->success)
				{
					FreeNode(exceptionSpec);
					exceptionSpec = GenerateNoSuccessAstNode();
					BacktrackTo(data, backtrackPosition);
				}

				backtrackPosition = data.CurrentToken;
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				if (!attributeSpecifierSeq->success)
				{
					FreeNode(attributeSpecifierSeq);
					attributeSpecifierSeq = GenerateNoSuccessAstNode();
					BacktrackTo(data, backtrackPosition);
				}

				backtrackPosition = data.CurrentToken;
				AstNode* trailingReturnType = ParseTrailingReturnType(data);
				if (!trailingReturnType->success)
				{
					FreeNode(trailingReturnType);
					trailingReturnType = GenerateNoSuccessAstNode();
					BacktrackTo(data, backtrackPosition);
				}

				return GenerateLambdaDeclaratorNode(parameterDeclarationClause, isMutable, exceptionSpec, attributeSpecifierSeq, trailingReturnType);
			}

			return GenerateNoSuccessAstNode();
		}

		// Postfix Expressions
		static AstNode* ParsePostfixExpression(ParserData& data)
		{
			// TODO: This one scares me test pretty good, otherwise I have a feeling you will get infinite loops which results in horrible lag
			// Try to look ahead and make sure we don't recurse further if it's not possible
			bool shouldRecurse = LookAheadBeforeSemicolon(data, { TokenType::LEFT_BRACKET, TokenType::LEFT_PAREN, TokenType::DOT, TokenType::ARROW, TokenType::PLUS_PLUS, TokenType::MINUS_MINUS });

			int backtrackPosition = data.CurrentToken;
			AstNode* primaryExpression = ParsePrimaryExpression(data);
			if (primaryExpression->success)
			{
				return primaryExpression;
			}
			FreeNode(primaryExpression);
			BacktrackTo(data, backtrackPosition);

			AstNode* simpleTypeSpecifier = ParseSimpleTypeSpecifier(data);
			if (simpleTypeSpecifier->success)
			{
				if (Match(data, TokenType::LEFT_PAREN))
				{
					// Optional
					AstNode* expressionList = ParseExpressionList(data);
					Consume(data, TokenType::RIGHT_PAREN);
					return GeneratePostfixSimpleTypeExpressionListNode(simpleTypeSpecifier, expressionList);
				}

				AstNode* bracedInitList = ParseBracedInitList(data);
				if (!bracedInitList->success)
				{
					FreeNode(simpleTypeSpecifier);
					FreeNode(bracedInitList);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				return GeneratePostfixSimpleTypeBraceListNode(simpleTypeSpecifier, bracedInitList);
			}
			FreeNode(simpleTypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* typenameSpecifier = ParseTypenameSpecifier(data);
			if (typenameSpecifier->success)
			{
				if (Match(data, TokenType::LEFT_PAREN))
				{
					// Optional
					AstNode* expressionList = ParseExpressionList(data);
					Consume(data, TokenType::RIGHT_PAREN);
					return GeneratePostfixTypenameSpecExpressionListNode(simpleTypeSpecifier, expressionList);
				}

				AstNode* bracedInitList = ParseBracedInitList(data);
				if (!bracedInitList->success)
				{
					FreeNode(simpleTypeSpecifier);
					FreeNode(bracedInitList);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				return GeneratePostfixTypenameSpecBraceListNode(simpleTypeSpecifier, bracedInitList);
			}
			FreeNode(typenameSpecifier);
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::KW_DYNAMIC_CAST))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::DynamicCast);
			}

			if (Match(data, TokenType::KW_STATIC_CAST))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::StaticCast);
			}

			if (Match(data, TokenType::KW_REINTERPRET_CAST))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::ReinterpretCast);
			}

			if (Match(data, TokenType::KW_CONST_CAST))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::ConstCast);
			}

			if (Match(data, TokenType::KW_TYPEID))
			{
				Consume(data, TokenType::LEFT_PAREN);
				int backtrackPos2 = data.CurrentToken;
				AstNode* expression = ParseExpression(data);
				if (expression->success)
				{
					Consume(data, TokenType::RIGHT_PAREN);
					return GeneratePostfixTypeIdExpressionNode(expression);
				}
				FreeNode(expression);
				BacktrackTo(data, backtrackPos2);

				AstNode* typeId = ParseTypeId(data);
				if (typeId->success)
				{
					Consume(data, TokenType::RIGHT_PAREN);
					return GeneratePostfixTypeIdNode(typeId);
				}
				FreeNode(typeId);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (!shouldRecurse)
			{
				return GenerateNoSuccessAstNode();
			}

			AstNode* postfixExpression = ParsePostfixExpression(data);
			if (Match(data, TokenType::LEFT_BRACKET))
			{
				int backtrackPosition2 = data.CurrentToken;
				AstNode* expression = ParseExpression(data);
				if (expression->success)
				{
					Consume(data, TokenType::RIGHT_BRACKET);
					return GeneratePostfixBracketExpressionNode(postfixExpression, expression);
				}
				FreeNode(expression);
				BacktrackTo(data, backtrackPosition2);

				// Optional
				AstNode* bracedInitList = ParseBracedInitList(data);
				Consume(data, TokenType::RIGHT_BRACKET);
				return GeneratePostfixBracketBraceListNode(postfixExpression, bracedInitList);
			}

			if (Match(data, TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GeneratePostfixParenExpressionListNode(postfixExpression, expressionList);
			}

			bool isDot = Match(data, TokenType::DOT);
			bool isArrow = Match(data, TokenType::ARROW);
			if (isDot || isArrow)
			{
				MemberOperatorType memberOp = isDot ? MemberOperatorType::DotOperator : MemberOperatorType::ArrowOperator;
				bool hasTemplateKeyword = Match(data, TokenType::KW_TEMPLATE);
				if (hasTemplateKeyword)
				{
					AstNode* idExpression = ParseIdExpression(data);
					if (!idExpression->success)
					{
						FreeNode(postfixExpression);
						return GenerateNoSuccessAstNode();
					}
					return GeneratePostfixMemberIdExpressionNode(postfixExpression, idExpression, hasTemplateKeyword, memberOp);
				}

				int backtrackPosition2 = data.CurrentToken;
				AstNode* idExpression = ParseIdExpression(data);
				if (idExpression->success)
				{
					return GeneratePostfixMemberIdExpressionNode(postfixExpression, idExpression, hasTemplateKeyword, memberOp);
				}
				FreeNode(idExpression);
				BacktrackTo(data, backtrackPosition2);

				AstNode* pseudoDestructorName = ParsePseudoDestructorName(data);
				return GeneratePostfixPseudoDestructorNode(postfixExpression, pseudoDestructorName, memberOp);
			}

			if (Match(data, TokenType::PLUS_PLUS))
			{
				return GeneratePostfixPlusPlusNode(postfixExpression);
			}

			if (Match(data, TokenType::MINUS_MINUS))
			{
				return GeneratePostfixMinusMinusNode(postfixExpression);
			}

			FreeNode(postfixExpression);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpressionList(ParserData& data)
		{
			return ParseInitializerList(data);
		}

		static AstNode* ParsePseudoDestructorName(ParserData& data)
		{
			if (Match(data, TokenType::TILDE))
			{
				AstNode* decltypeSpecifier = ParseDecltypeSpecifier(data);
				return GeneratePseudoDestructorDecltypeNode(decltypeSpecifier);
			}

			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				if (!nestedNameSpecifier->success)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::TILDE);
				AstNode* typeName = ParseTypeName(data);
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(simpleTemplateId);
					FreeNode(typeName);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoDestructorTemplateNode(nestedNameSpecifier, simpleTemplateId, typeName);
			}

			// Nested name specifier is optional at this point
			if (Match(data, TokenType::TILDE))
			{
				AstNode* typeName = ParseTypeName(data);
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(typeName);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoDestructorNode(nestedNameSpecifier, typeName);
			}

			AstNode* nestedTypeName = ParseTypeName(data);
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
				Consume(data, TokenType::TILDE);
				AstNode* typeName = ParseTypeName(data);
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(nestedTypeName);
					FreeNode(typeName);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoNestedDestructorNode(nestedNameSpecifier, nestedTypeName, typeName);
			}

			FreeNode(nestedTypeName);
			FreeNode(nestedNameSpecifier);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Unary Expressions
		static AstNode* ParseUnaryExpression(ParserData& data)
		{
			int backtrackCursor = data.CurrentToken;
			AstNode* postfix = ParsePostfixExpression(data);
			if (postfix->success)
			{
				return postfix;
			}
			FreeNode(postfix);
			BacktrackTo(data, backtrackCursor);

			if (Peek(data) == TokenType::PLUS_PLUS || Peek(data) == TokenType::MINUS_MINUS || Peek(data) == TokenType::STAR || Peek(data) == TokenType::AND ||
				Peek(data) == TokenType::PLUS || Peek(data) == TokenType::MINUS || Peek(data) == TokenType::BANG || Peek(data) == TokenType::TILDE)
			{
				return GenerateUnaryExpressionNode(ParseOverloadableOperator(data), ParseCastExpression(data));
			}

			if (Match(data, TokenType::KW_SIZEOF))
			{
				if (Match(data, TokenType::LEFT_PAREN))
				{
					AstNode* typeId = ParseTypeId(data);
					Consume(data, TokenType::RIGHT_PAREN);
					return GenerateSizeofExpressionNode(typeId);
				}

				if (Match(data, TokenType::DOT))
				{
					Consume(data, TokenType::DOT);
					Consume(data, TokenType::DOT);
					Consume(data, TokenType::LEFT_PAREN);
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					Consume(data, TokenType::RIGHT_PAREN);
					return GenerateSizeofIdentifierExpressionNode(identifier);
				}

				return GenerateSizeofExpressionNode(ParseUnaryExpression(data));
			}

			AstNode* alignmentExpression = ParseAlignmentExpression(data);
			if (alignmentExpression->success)
			{
				return alignmentExpression;
			}
			FreeNode(alignmentExpression);
			BacktrackTo(data, backtrackCursor);

			AstNode* noexceptExpr = ParseNoexceptExpression(data);
			if (noexceptExpr->success)
			{
				return noexceptExpr;
			}
			FreeNode(noexceptExpr);
			BacktrackTo(data, backtrackCursor);

			AstNode* newExpr = ParseNewExpression(data);
			if (newExpr->success)
			{
				return newExpr;
			}
			FreeNode(newExpr);
			BacktrackTo(data, backtrackCursor);

			AstNode* deleteExpr = ParseDeleteExpression(data);
			if (deleteExpr->success)
			{
				return deleteExpr;
			}
			FreeNode(deleteExpr);
			BacktrackTo(data, backtrackCursor);

			return GenerateNoSuccessAstNode();
		}

		// New Expressions
		static AstNode* ParseNewExpression(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
			}

			if (Match(data, TokenType::KW_NEW))
			{
				// Optional
				AstNode* newPlacement = ParseNewPlacement(data);
				if (Match(data, TokenType::LEFT_PAREN))
				{
					AstNode* typeId = ParseTypeId(data);
					Consume(data, TokenType::RIGHT_PAREN);

					// Optional
					AstNode* newInitializer = ParseNewInitializer(data);
					return GenerateNewExpressionNode(newPlacement, typeId, newInitializer);
				}

				AstNode* newTypeId = ParseNewTypeId(data);
				if (!newTypeId->success)
				{
					FreeNode(newPlacement);
					FreeNode(newTypeId);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				// Optional
				AstNode* newInitializer = ParseNewInitializer(data);
				return GenerateNewTypeIdExpressionNode(newPlacement, newTypeId, newInitializer);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewPlacement(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* expressionList = ParseExpressionList(data);
				if (!expressionList->success)
				{
					FreeNode(expressionList);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateNewPlacementNode(expressionList);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewTypeId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence(data);
			if (!typeSpecifierSeq->success)
			{
				FreeNode(typeSpecifierSeq);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* newDeclarator = ParseNewDeclarator(data);
			return GenerateNewTypeIdNode(typeSpecifierSeq, newDeclarator);
		}

		static AstNode* ParseNewDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* noptrNewDeclarator = ParseNoptrNewDeclarator(data);
			if (noptrNewDeclarator->success)
			{
				return noptrNewDeclarator;
			}
			FreeNode(noptrNewDeclarator);
			BacktrackTo(data, backtrackPosition);

			AstNode* ptrOperator = ParsePtrOperator(data);
			if (!ptrOperator->success)
			{
				FreeNode(ptrOperator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* newDeclarator = ParseNewDeclarator(data);
			return GenerateNewDeclaratorNode(ptrOperator, newDeclarator);
		}

		static AstNode* ParseNoptrNewDeclarator(ParserData& data)
		{
			// TODO: this recursion is crazy
			int backtrackPosition = data.CurrentToken;
			if (!Match(data, TokenType::LEFT_BRACKET))
			{
				return GenerateNoSuccessAstNode();
			}

			AstNode* expression = ParseExpression(data);
			if (expression->success)
			{
				Consume(data, TokenType::RIGHT_BRACKET);

				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				return GenerateNoptrNewTailDeclaratorNode(expression, attributeSpecifierSeq);
			}
			FreeNode(expression);
			BacktrackTo(data, backtrackPosition);

			Consume(data, TokenType::LEFT_BRACKET);
			AstNode* constantExpression = ParseConstantExpression(data);
			if (constantExpression->success)
			{
				Consume(data, TokenType::RIGHT_BRACKET);
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);

				AstNode* noptrNewDeclarator = ParseNoptrNewDeclarator(data);
				if (!noptrNewDeclarator->success)
				{
					FreeNode(noptrNewDeclarator);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateNoptrNewDeclaratorNode(noptrNewDeclarator, constantExpression, attributeSpecifierSeq);
			}

			FreeNode(constantExpression);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewInitializer(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList(data);
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateNewInitializerNode(expressionList);
			}

			return ParseBracedInitList(data);
		}

		// Delete
		static AstNode* ParseDeleteExpression(ParserData& data)
		{
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
			}

			if (Match(data, TokenType::KW_DELETE))
			{
				bool deleteArr = false;
				if (Match(data, TokenType::LEFT_BRACKET))
				{
					Consume(data, TokenType::RIGHT_BRACKET);
					deleteArr = true;
				}

				return GenerateDeleteNode(ParseCastExpression(data), deleteArr);
			}

			return GenerateNoSuccessAstNode();
		}

		// Noexcept
		static AstNode* ParseNoexceptExpression(ParserData& data)
		{
			if (Match(data, TokenType::KW_NOEXCEPT))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return expression;
			}

			return GenerateNoSuccessAstNode();
		}

		// Cast
		static AstNode* ParseCastExpression(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* typeId = ParseTypeId(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GenerateCastExpressionNode(typeId, ParseCastExpression(data));
			}

			return ParseUnaryExpression(data);
		}

		// PointerToMember Expression
		static AstNode* ParsePmExpression(ParserData& data)
		{
			AstNode* result = ParseCastExpression(data);

			// TODO: Does it matter that I'm doing left recursion and he does right recursion????
			while (Match(data, TokenType::POINTER_TO_MEMBER))
			{
				AstNode* left = ParsePmExpression(data);
				result = GeneratePointerToMemberNode(left, result);
			}

			return result;
		}

		// Primary operations
		static AstNode* ParseMultiplicativeExpression(ParserData& data)
		{
			AstNode* result = ParsePmExpression(data);

			while (Peek(data) == TokenType::STAR || Peek(data) == TokenType::DIV || Peek(data) == TokenType::MODULO)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseMultiplicativeExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseAdditiveExpression(ParserData& data)
		{
			AstNode* result = ParseMultiplicativeExpression(data);

			while (Peek(data) == TokenType::PLUS || Peek(data) == TokenType::MINUS)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseAdditiveExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseShiftExpression(ParserData& data)
		{
			AstNode* result = ParseAdditiveExpression(data);

			while (Peek(data) == TokenType::LEFT_SHIFT || Peek(data) == TokenType::RIGHT_SHIFT)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseShiftExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Comparision operations
		static AstNode* ParseRelationalExpression(ParserData& data)
		{
			AstNode* result = ParseShiftExpression(data);

			while (Peek(data) == TokenType::LEFT_ANGLE_BRACKET || Peek(data) == TokenType::RIGHT_ANGLE_BRACKET || Peek(data) == TokenType::LESS_THAN_EQ || Peek(data) == TokenType::GREATER_THAN_EQ)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseRelationalExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseEqualityExpression(ParserData& data)
		{
			AstNode* result = ParseRelationalExpression(data);

			while (Peek(data) == TokenType::EQUAL_EQUAL || Peek(data) == TokenType::BANG_EQUAL)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseEqualityExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Logical operations
		static AstNode* ParseAndExpression(ParserData& data)
		{
			AstNode* result = ParseEqualityExpression(data);

			while (Peek(data) == TokenType::AND)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseAndExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseExclusiveOrExpression(ParserData& data)
		{
			AstNode* result = ParseAndExpression(data);

			while (Peek(data) == TokenType::CARET)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseExclusiveOrExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseInclusiveOrExpression(ParserData& data)
		{
			AstNode* result = ParseExclusiveOrExpression(data);

			while (Peek(data) == TokenType::PIPE)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseInclusiveOrExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseLogicalAndExpression(ParserData& data)
		{
			AstNode* result = ParseInclusiveOrExpression(data);

			while (Peek(data) == TokenType::LOGICAL_AND)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseLogicalAndExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseLogicalOrExpression(ParserData& data)
		{
			AstNode* result = ParseLogicalAndExpression(data);

			while (Peek(data) == TokenType::LOGICAL_OR)
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				AstNode* right = ParseLogicalOrExpression(data);
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Misc expressions
		static AstNode* ParseConditionalExpression(ParserData& data)
		{
			AstNode* result = ParseLogicalOrExpression(data);

			if (Match(data, TokenType::QUESTION))
			{
				AstNode* ifTrueNode = ParseExpression(data);
				Consume(data, TokenType::SEMICOLON);
				AstNode* ifFalseNode = ParseAssignmentExpression(data);
				result = GenerateTernaryExpressionNode(result, ifTrueNode, ifFalseNode);
			}

			return result;
		}

		static AstNode* ParseAssignmentExpression(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseConditionalExpression(data);

			if (result->success)
			{
				return result;
			}
			FreeNode(result);
			BacktrackTo(data, backtrackPosition);

			result = ParseLogicalOrExpression(data);
			if (IsAssignmentOperator(Peek(data)))
			{
				AssignmentOperatorType assignmentType = AssignmentOperatorType::None;
				switch (Peek(data))
				{
				case TokenType::EQUAL:
					assignmentType = AssignmentOperatorType::Equal;
					break;
				case TokenType::STAR_EQUAL:
					assignmentType = AssignmentOperatorType::TimesEqual;
					break;
				case TokenType::DIV_EQUAL:
					assignmentType = AssignmentOperatorType::DivEqual;
					break;
				case TokenType::MODULO_EQUAL:
					assignmentType = AssignmentOperatorType::ModEqual;
					break;
				case TokenType::PLUS_EQUAL:
					assignmentType = AssignmentOperatorType::PlusEqual;
					break;
				case TokenType::MINUS_EQUAL:
					assignmentType = AssignmentOperatorType::MinusEqual;
					break;
				case TokenType::RIGHT_SHIFT_EQUAL:
					assignmentType = AssignmentOperatorType::RightShiftEqual;
					break;
				case TokenType::LEFT_SHIFT_EQUAL:
					assignmentType = AssignmentOperatorType::LeftShiftEqual;
					break;
				case TokenType::AND_EQUAL:
					assignmentType = AssignmentOperatorType::AndEqual;
					break;
				case TokenType::CARET_EQUAL:
					assignmentType = AssignmentOperatorType::XorEqual;
					break;
				case TokenType::PIPE_EQUAL:
					assignmentType = AssignmentOperatorType::OrEqual;
					break;
				}
				result = GenerateAssignmentExpressionNode(result, assignmentType, ParseInitializerClause(data));
				return result;
			}
			FreeNode(result);
			BacktrackTo(data, backtrackPosition);

			return ParseThrowExpression(data);
		}

		static AstNode* ParseAlignmentExpression(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Peek(data) == TokenType::KW_ALIGN_OF)
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* typeId = ParseTypeId(data);
				if (typeId->success)
				{
					Consume(data, TokenType::RIGHT_PAREN);
					return GenerateAlignmentExpressionNode(typeId);
				}
				FreeNode(typeId);
				BacktrackTo(data, backtrackPosition);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpression(ParserData& data)
		{
			AstNode* expression = ParseAssignmentExpression(data);

			while (Match(data, TokenType::COMMA))
			{
				AstNode* nextExpression = expression;
				AstNode* expression = GenerateAstNode(AstNodeType::Expression);
				expression->expressionNode.expression = ParseExpression(data);
				expression->expressionNode.nextExpression = nextExpression;
			}

			return expression;
		}

		static AstNode* ParseConstantExpression(ParserData& data)
		{
			return GenerateConstantExpressionNode(ParseConditionalExpression(data));
		}

		// Statements
		static AstNode* ParseStatement(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* labeledStatement = ParseLabeledStatement(data);
			if (labeledStatement->success)
			{
				return GenerateStatementNode(GenerateNoSuccessAstNode(), labeledStatement);
			}
			FreeNode(labeledStatement);
			BacktrackTo(data, backtrackPosition);

			AstNode* declarationStatement = ParseDeclarationStatement(data);
			if (declarationStatement->success)
			{
				return GenerateStatementNode(GenerateNoSuccessAstNode(), declarationStatement);
			}
			FreeNode(declarationStatement);
			BacktrackTo(data, backtrackPosition);

			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			int backtrackPosition2 = data.CurrentToken;

			AstNode* expressionStatement = ParseExpressionStatement(data);
			if (expressionStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, expressionStatement);
			}
			FreeNode(expressionStatement);
			BacktrackTo(data, backtrackPosition2);

			AstNode* compoundStatement = ParseCompoundStatement(data);
			if (compoundStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, compoundStatement);
			}
			FreeNode(compoundStatement);
			BacktrackTo(data, backtrackPosition2);

			AstNode* selectionStatement = ParseSelectionStatement(data);
			if (selectionStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, selectionStatement);
			}
			FreeNode(selectionStatement);
			BacktrackTo(data, backtrackPosition2);

			AstNode* iterationStatement = ParseIterationStatement(data);
			if (iterationStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, iterationStatement);
			}
			FreeNode(iterationStatement);
			BacktrackTo(data, backtrackPosition2);

			AstNode* jumpStatement = ParseJumpStatement(data);
			if (jumpStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, jumpStatement);
			}
			FreeNode(jumpStatement);
			BacktrackTo(data, backtrackPosition2);

			AstNode* tryBlock = ParseTryBlock(data);
			if (tryBlock->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, tryBlock);
			}
			FreeNode(tryBlock);
			FreeNode(attributeSpecifierSeq);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLabeledStatement(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);

			if (Peek(data) == TokenType::IDENTIFIER)
			{
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				Consume(data, TokenType::COLON);
				AstNode* statement = ParseStatement(data);
				return GenerateLabeledIdentifierNode(attributeSpecifierSeq, identifier, statement);
			}

			if (Match(data, TokenType::KW_CASE))
			{
				AstNode* constantExpression = ParseConstantExpression(data);
				Consume(data, TokenType::COLON);
				AstNode* statement = ParseStatement(data);
				return GenerateCaseLabelNode(attributeSpecifierSeq, constantExpression, statement);
			}

			if (Match(data, TokenType::KW_DEFAULT))
			{
				Consume(data, TokenType::COLON);
				AstNode* statement = ParseStatement(data);
				return GenerateDefaultLabelNode(attributeSpecifierSeq, statement);
			}

			FreeNode(attributeSpecifierSeq);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpressionStatement(ParserData& data)
		{
			if (Match(data, TokenType::SEMICOLON))
			{
				return GenerateEmptyStatementNode();
			}

			int backtrackPosition = data.CurrentToken;
			AstNode* expression = ParseExpression(data);
			if (expression->success)
			{
				Consume(data, TokenType::SEMICOLON);
				return expression;
			}

			FreeNode(expression);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCompoundStatement(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_CURLY_BRACKET))
			{
				// Optional
				AstNode* statementSequence = ParseStatementSequence(data);
				Consume(data, TokenType::RIGHT_CURLY_BRACKET);
				return GenerateCompoundStatementNode(statementSequence);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseStatementSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseStatement(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextStatement = nullptr;
			do
			{
				backtrackPosition = data.CurrentToken;
				AstNode* nextStatement = ParseStatement(data);
				result = GenerateStatementSequenceNode(result, nextStatement->success ? nextStatement : GenerateNoSuccessAstNode());
			} while (nextStatement && nextStatement->success);

			Logger::Assert(nextStatement != nullptr, "Something went horribly wrong when parsing a statement sequence.");
			FreeNode(nextStatement);
			BacktrackTo(data, backtrackPosition);
			return result;
		}

		// Selection statements
		static AstNode* ParseSelectionStatement(ParserData& data)
		{
			if (Match(data, TokenType::KW_IF))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition(data);
				Consume(data, TokenType::RIGHT_PAREN);

				AstNode* ifStatement = ParseStatement(data);
				AstNode* elseStatement = Match(data, TokenType::KW_ELSE) ?
					ParseStatement(data) :
					GenerateNoSuccessAstNode();

				return GenerateIfElseNode(condition, ifStatement, elseStatement);
			}

			if (Match(data, TokenType::KW_SWITCH))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition(data);
				Consume(data, TokenType::RIGHT_PAREN);

				AstNode* statement = ParseStatement(data);
				return GenerateSwitchNode(condition, statement);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCondition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* expression = ParseExpression(data);
			if (expression->success)
			{
				return expression;
			}
			BacktrackTo(data, backtrackPosition);
			FreeNode(expression);

			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence(data);
			if (!declSpecifierSeq->success)
			{
				BacktrackTo(data, backtrackPosition);
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				return GenerateNoSuccessAstNode();
			}

			AstNode* declarator = ParseDeclarator(data);
			if (Match(data, TokenType::EQUAL))
			{
				AstNode* initializerClause = ParseInitializerClause(data);
				return GenerateInitializerConditionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, initializerClause);
			}

			AstNode* bracedInitList = ParseBracedInitList(data);
			return GenerateBracedInitConditionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, bracedInitList);
		}

		// Iteration statements
		static AstNode* ParseIterationStatement(ParserData& data)
		{
			if (Match(data, TokenType::KW_WHILE))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition(data);
				Consume(data, TokenType::RIGHT_PAREN);
				AstNode* statement = ParseStatement(data);

				return GenerateWhileLoopNode(condition, statement);
			}

			if (Match(data, TokenType::KW_DO))
			{
				AstNode* statement = ParseStatement(data);
				Consume(data, TokenType::KW_WHILE);
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* condition = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				Consume(data, TokenType::SEMICOLON);

				return GenerateDoWhileLoopNode(statement, condition);
			}

			if (Match(data, TokenType::KW_FOR))
			{
				Consume(data, TokenType::LEFT_PAREN);
				int backtrackPosition = data.CurrentToken;
				AstNode* forInitStatement = ParseForInitStatement(data);
				if (forInitStatement->success)
				{
					// This is optional
					AstNode* condition = ParseCondition(data);
					Consume(data, TokenType::SEMICOLON);
					// This is also optional
					AstNode* expression = ParseExpression(data);
					Consume(data, TokenType::RIGHT_PAREN);
					AstNode* statement = ParseStatement(data);

					return GenerateForLoopNode(forInitStatement, condition, expression, statement);
				}
				FreeNode(forInitStatement);
				BacktrackTo(data, backtrackPosition);

				AstNode* forRangeDeclaration = ParseForRangeDeclaration(data);
				Consume(data, TokenType::COLON);
				AstNode* forRangeInitializer = ParseForRangeInitializer(data);
				Consume(data, TokenType::RIGHT_PAREN);
				AstNode* statement = ParseStatement(data);

				return GenerateForEachLoopNode(forRangeDeclaration, forRangeInitializer, statement);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseForInitStatement(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* expressionStatement = ParseExpressionStatement(data);
			if (expressionStatement->success)
			{
				return expressionStatement;
			}
			FreeNode(expressionStatement);
			BacktrackTo(data, backtrackPosition);

			AstNode* simpleDeclaration = ParseSimpleDeclaration(data);
			if (simpleDeclaration->success)
			{
				return simpleDeclaration;
			}

			FreeNode(simpleDeclaration);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseForRangeDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence(data);
			if (!typeSpecifierSeq->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(typeSpecifierSeq);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* declarator = ParseDeclarator(data);
			if (!declarator->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(typeSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateForRangeDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
		}

		static AstNode* ParseForRangeInitializer(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* expression = ParseExpression(data);
			if (!expression->success)
			{
				FreeNode(expression);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// TODO: For each loop seems weird do some good testing on this piece of grammar
			AstNode* bracedInitList = ParseBracedInitList(data);
			if (!bracedInitList->success)
			{
				FreeNode(expression);
				FreeNode(bracedInitList);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateForRangeInitializerNode(expression, bracedInitList);
		}

		// Jump statements
		static AstNode* ParseJumpStatement(ParserData& data)
		{
			if (Match(data, TokenType::KW_BREAK))
			{
				Consume(data, TokenType::SEMICOLON);
				return GenerateBreakNode();
			}

			if (Match(data, TokenType::KW_CONTINUE))
			{
				Consume(data, TokenType::KW_CONTINUE);
				return GenerateContinueNode();
			}

			if (Match(data, TokenType::KW_RETURN))
			{
				if (Match(data, TokenType::SEMICOLON))
				{
					return GenerateReturnNode(GenerateNoSuccessAstNode());
				}

				int backtrackPosition = data.CurrentToken;
				AstNode* expression = ParseExpression(data);
				if (expression->success)
				{
					Consume(data, TokenType::SEMICOLON);
					return GenerateReturnNode(expression);
				}
				FreeNode(expression);
				BacktrackTo(data, backtrackPosition);

				AstNode* bracedInitList = ParseBracedInitList(data);
				if (bracedInitList->success)
				{
					Consume(data, TokenType::SEMICOLON);
					return GenerateReturnNode(bracedInitList);
				}
				FreeNode(bracedInitList);
				BacktrackTo(data, backtrackPosition);

				return GenerateNoSuccessAstNode();
			}

			if (Match(data, TokenType::KW_GOTO))
			{
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				Consume(data, TokenType::SEMICOLON);
				return GenerateGotoNode(identifier);
			}

			return GenerateNoSuccessAstNode();
		}

		// Declarations
		static AstNode* ParseDeclarationStatement(ParserData& data)
		{
			return ParseBlockDeclaration(data);
		}

		static AstNode* ParseDeclarationSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseDeclaration(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclaration = ParseDeclarationSequence(data);
			result = GenerateDeclarationSeqNode(result, nextDeclaration);

			return result;
		}

		static AstNode* ParseUSystemDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::USYSTEM))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause(data);
				if (parameterDeclarationClause->success)
				{
					Consume(data, TokenType::RIGHT_PAREN);
					Match(data, TokenType::SEMICOLON);
					AstNode* namedNamespaceDefinition = ParseNamedNamespaceDefinition(data);
					if (namedNamespaceDefinition->success)
					{
						return GenerateUSystemDeclarationNode(parameterDeclarationClause, namedNamespaceDefinition);
					}
					FreeNode(namedNamespaceDefinition);
				}
				FreeNode(parameterDeclarationClause);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* blockDeclaration = ParseBlockDeclaration(data);
			if (blockDeclaration->success)
			{
				return blockDeclaration;
			}
			FreeNode(blockDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* functionDefinition = ParseFunctionDefinition(data);
			if (functionDefinition->success)
			{
				return functionDefinition;
			}
			FreeNode(functionDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* templateDeclaration = ParseTemplateDeclaration(data);
			if (templateDeclaration->success)
			{
				return templateDeclaration;
			}
			FreeNode(templateDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* explicitInstantiation = ParseExplicitInstantiation(data);
			if (explicitInstantiation->success)
			{
				return explicitInstantiation;
			}
			FreeNode(explicitInstantiation);
			BacktrackTo(data, backtrackPosition);

			AstNode* explicitSpecialization = ParseExplicitSpecialization(data);
			if (explicitSpecialization->success)
			{
				return explicitSpecialization;
			}
			FreeNode(explicitSpecialization);
			BacktrackTo(data, backtrackPosition);

			AstNode* linkageSpecification = ParseLinkageSpecification(data);
			if (linkageSpecification->success)
			{
				return linkageSpecification;
			}
			FreeNode(linkageSpecification);
			BacktrackTo(data, backtrackPosition);

			AstNode* namespaceDefinition = ParseNamespaceDefinition(data);
			if (namespaceDefinition->success)
			{
				return namespaceDefinition;
			}
			FreeNode(namespaceDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* emptyDeclaration = ParseEmptyDeclaration(data);
			if (emptyDeclaration->success)
			{
				return emptyDeclaration;
			}
			FreeNode(emptyDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* attributeDeclaration = ParseAttributeDeclaration(data);
			if (attributeDeclaration->success)
			{
				return attributeDeclaration;
			}
			FreeNode(attributeDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* uSystemDeclaration = ParseUSystemDeclaration(data);
			if (uSystemDeclaration->success)
			{
				return uSystemDeclaration;
			}
			FreeNode(uSystemDeclaration);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBlockDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* simpleDeclaration = ParseSimpleDeclaration(data);
			if (simpleDeclaration->success)
			{
				return simpleDeclaration;
			}
			FreeNode(simpleDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* asmDefinition = ParseAsmDefinition(data);
			if (asmDefinition->success)
			{
				return asmDefinition;
			}
			FreeNode(asmDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* namespaceAliasDefinition = ParseNamespaceAliasDefinition(data);
			if (namespaceAliasDefinition->success)
			{
				return namespaceAliasDefinition;
			}
			FreeNode(namespaceAliasDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* usingDeclaration = ParseUsingDeclaration(data);
			if (usingDeclaration->success)
			{
				return usingDeclaration;
			}
			FreeNode(usingDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* usingDirective = ParseUsingDirective(data);
			if (usingDirective->success)
			{
				return usingDirective;
			}
			FreeNode(usingDirective);
			BacktrackTo(data, backtrackPosition);

			AstNode* staticAssertDeclaration = ParseStaticAssertDeclaration(data);
			if (staticAssertDeclaration->success)
			{
				return staticAssertDeclaration;
			}
			FreeNode(staticAssertDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* aliasDeclaration = ParseAliasDeclaration(data);
			if (aliasDeclaration->success)
			{
				return aliasDeclaration;
			}
			FreeNode(aliasDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* opaqueEnumDeclaration = ParseOpaqueEnumDeclaration(data);
			if (opaqueEnumDeclaration->success)
			{
				return opaqueEnumDeclaration;
			}
			FreeNode(opaqueEnumDeclaration);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAliasDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_USING))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					Consume(data, TokenType::EQUAL);
					AstNode* typeId = ParseTypeId(data);
					if (typeId->success)
					{
						Consume(data, TokenType::SEMICOLON);
						return GenerateAliasDeclarationNode(identifier, typeId);
					}
					FreeNode(typeId);
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseSimpleDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// All optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence(data);
			AstNode* initDeclaratorList = ParseInitDeclaratorList(data);
			if (!(attributeSpecifierSeq->success || declSpecifierSeq->success || initDeclaratorList->success))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(initDeclaratorList);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(data, TokenType::SEMICOLON))
			{
				return GenerateSimpleDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, initDeclaratorList);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			FreeNode(initDeclaratorList);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseStaticAssertDeclaration(ParserData& data)
		{
			if (Match(data, TokenType::KW_STATIC_ASSERT))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* constantExpression = ParseConstantExpression(data);
				Consume(data, TokenType::COMMA);
				Token stringLiteral = ConsumeCurrent(data, TokenType::STRING_LITERAL);
				Consume(data, TokenType::RIGHT_PAREN);
				Consume(data, TokenType::SEMICOLON);

				return GenerateStaticAssertDeclarationNode(constantExpression, stringLiteral);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEmptyDeclaration(ParserData& data)
		{
			if (Match(data, TokenType::SEMICOLON))
			{
				return GenerateEmptyStatementNode();
			}
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			if (!attributeSpecifierSeq->success)
			{
				FreeNode(attributeSpecifierSeq);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}
			Consume(data, TokenType::SEMICOLON);

			return attributeSpecifierSeq;
		}

		static AstNode* ParseDeclarationSpecifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_FRIEND || Peek(data) == TokenType::KW_TYPEDEF || Peek(data) == TokenType::KW_CONST_EXPR)
			{
				return GenerateSimpleDeclSpecifierNode(ConsumeCurrent(data, Peek(data)));
			}

			int backtrackPosition = data.CurrentToken;
			AstNode* storageClassSpecifier = ParseStorageClassSpecifier(data);
			if (storageClassSpecifier->success)
			{
				return GenerateDeclSpecifierNode(storageClassSpecifier);
			}
			FreeNode(storageClassSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* typeSpecifier = ParseTypeSpecifier(data);
			if (typeSpecifier->success)
			{
				return GenerateDeclSpecifierNode(typeSpecifier);
			}
			FreeNode(typeSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* functionSpecifier = ParseFunctionSpecifier(data);
			if (functionSpecifier->success)
			{
				return GenerateDeclSpecifierNode(functionSpecifier);
			}
			FreeNode(functionSpecifier);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclarationSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseDeclarationSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclSpec = ParseDeclarationSpecifierSequence(data);
			result = GenerateDeclSpecSeqNode(result, nextDeclSpec, GenerateNoSuccessAstNode());
			if (!nextDeclSpec->success)
			{
				FreeNode(result->declSpecSeq.attributeSpecifierSeq);
				// Optional
				result->declSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			}

			return result;
		}

		static AstNode* ParseStorageClassSpecifier(ParserData& data)
		{
			if (PeekIn(data, { TokenType::KW_AUTO, TokenType::KW_REGISTER, TokenType::KW_STATIC, TokenType::KW_THREAD_LOCAL, TokenType::KW_EXTERN, TokenType::KW_MUTABLE }))
			{
				return GenerateStorageClassSpecNode(ConsumeCurrent(data, Peek(data)));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionSpecifier(ParserData& data)
		{
			if (PeekIn(data, { TokenType::KW_INLINE, TokenType::KW_VIRTUAL, TokenType::KW_EXPLICIT }))
			{
				return GenerateFunctionSpecNode(ConsumeCurrent(data, Peek(data)));
			}

			return GenerateNoSuccessAstNode();
		}

		// Types/typedefs
		static AstNode* ParseTypedefName(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateTypedefNameNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// TODO: Is this right?
			if (PeekIn(data, { TokenType::KW_CLASS, TokenType::KW_UNION, TokenType::KW_STRUCT, TokenType::KW_ENUM }) &&
				LookAheadBeforeSemicolon(data, { TokenType::LEFT_CURLY_BRACKET }))
			{
				AstNode* classSpecifier = ParseClassSpecifier(data);
				if (classSpecifier->success)
				{
					return classSpecifier;
				}
				FreeNode(classSpecifier);
				BacktrackTo(data, backtrackPosition);

				AstNode* enumSpecifier = ParseEnumSpecifier(data);
				if (enumSpecifier->success)
				{
					return enumSpecifier;
				}
				FreeNode(enumSpecifier);
				BacktrackTo(data, backtrackPosition);
			}

			AstNode* trailingTypeSpecifier = ParseTrailingTypeSpecifier(data);
			if (trailingTypeSpecifier->success)
			{
				return trailingTypeSpecifier;
			}
			FreeNode(trailingTypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTrailingTypeSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* simpleTypeSpecifier = ParseSimpleTypeSpecifier(data);
			if (simpleTypeSpecifier->success)
			{
				return simpleTypeSpecifier;
			}
			FreeNode(simpleTypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* elaboratedTypeSpecifier = ParseElaboratedTypeSpecifier(data);
			if (elaboratedTypeSpecifier->success)
			{
				return elaboratedTypeSpecifier;
			}
			FreeNode(elaboratedTypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* typenameSpecifier = ParseTypenameSpecifier(data);
			if (typenameSpecifier->success)
			{
				return typenameSpecifier;
			}
			FreeNode(typenameSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* cvQualifier = ParseCvQualifier(data);
			if (cvQualifier->success)
			{
				return cvQualifier;
			}
			FreeNode(cvQualifier);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseTypeSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextTypeSpecifier = ParseTypeSpecifierSequence(data);
			result = GenerateTypeSpecSeqNode(result, nextTypeSpecifier, GenerateNoSuccessAstNode());
			if (!nextTypeSpecifier->success)
			{
				FreeNode(result->typeSpecSeq.attributeSpecifierSeq);
				result->typeSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			}

			return result;
		}

		static AstNode* ParseTrailingTypeSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseTrailingTypeSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextTypeSpecifier = ParseTrailingTypeSpecifierSequence(data);
			result = GenerateTrailingTypeSpecSeqNode(result, nextTypeSpecifier, GenerateNoSuccessAstNode());
			if (!nextTypeSpecifier->success)
			{
				FreeNode(result->trailingTypeSpecSeq.attributeSpecifierSeq);
				result->trailingTypeSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			}

			return result;
		}

		static AstNode* ParseSimpleTypeSpecifier(ParserData& data)
		{
			if (PeekIn(data, { TokenType::KW_CHAR, TokenType::KW_CHAR16_T, TokenType::KW_CHAR32_T, TokenType::KW_WCHAR_T, TokenType::KW_BOOL, TokenType::KW_SHORT, TokenType::KW_INT,
				TokenType::KW_LONG, TokenType::KW_SIGNED, TokenType::KW_UNSIGNED, TokenType::KW_FLOAT, TokenType::KW_DOUBLE, TokenType::KW_VOID, TokenType::KW_AUTO }))
			{
				return GenerateSimpleTypeTokenSpecNode(ConsumeCurrent(data, Peek(data)));
			}

			int backtrackPosition = data.CurrentToken;
			AstNode* decltypeSpecifier = ParseDecltypeSpecifier(data);
			if (decltypeSpecifier->success)
			{
				return decltypeSpecifier;
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				if (!nestedNameSpecifier->success)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(data, backtrackPosition);
				}
				AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
				if (simpleTemplateId->success)
				{
					return GenerateSimpleTypeTemplateSpecNode(nestedNameSpecifier, simpleTemplateId);
				}
				FreeNode(simpleTemplateId);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* typeName = ParseTypeName(data);
			if (typeName->success)
			{
				return GenerateSimpleTypeSpecNode(nestedNameSpecifier, typeName);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(typeName);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeName(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
			if (simpleTemplateId->success)
			{
				return simpleTemplateId;
			}
			FreeNode(simpleTemplateId);
			BacktrackTo(data, backtrackPosition);

			AstNode* className = ParseClassName(data);
			if (className->success)
			{
				return className;
			}
			FreeNode(className);
			BacktrackTo(data, backtrackPosition);

			AstNode* enumName = ParseEnumName(data);
			if (enumName->success)
			{
				return enumName;
			}
			FreeNode(enumName);
			BacktrackTo(data, backtrackPosition);

			AstNode* typedefName = ParseTypedefName(data);
			if (typedefName->success)
			{
				return typedefName;
			}
			FreeNode(typedefName);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDecltypeSpecifier(ParserData& data)
		{
			if (Match(data, TokenType::KW_DECLTYPE))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return GenerateDecltypeSpecNode(expression);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseElaboratedTypeSpecifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_ENUM)
			{
				bool hasScopeOp = Match(data, TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(data, TokenType::COLON);
				}

				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				return GenerateElaboratedSpecifierEnumNode(nestedNameSpecifier, identifier, hasScopeOp);
			}

			int backtrackPosition = data.CurrentToken;
			AstNode* classKey = ParseClassKey(data);
			if (classKey->success)
			{
				int backtrackPosition2 = data.CurrentToken;
				bool hasScopeOp = Match(data, TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(data, TokenType::COLON);
				}

				// TODO: Test if this actually works right...
				bool isTemplate = LookAheadBeforeSemicolon(data, { TokenType::LEFT_ANGLE_BRACKET });
				if (isTemplate)
				{
					// Optional
					AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
					if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
					{
						FreeNode(nestedNameSpecifier);
						nestedNameSpecifier = ParseNestedNameSpecifier(data);
					}
					bool hasTemplateKeyword = Match(data, TokenType::KW_TEMPLATE);
					AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
					if (simpleTemplateId->success)
					{
						return GenerateElaboratedSpecifierTemplateNode(classKey, nestedNameSpecifier, simpleTemplateId, hasScopeOp, hasTemplateKeyword);
					}
					FreeNode(simpleTemplateId);
					FreeNode(nestedNameSpecifier);
					BacktrackTo(data, backtrackPosition2);
				}

				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				hasScopeOp = Match(data, TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(data, TokenType::COLON);
				}

				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					return GenerateElaboratedSpecifierClassNode(classKey, attributeSpecifierSeq, nestedNameSpecifier, identifier, hasScopeOp);
				}
				FreeNode(nestedNameSpecifier);
				FreeNode(attributeSpecifierSeq);
			}

			FreeNode(classKey);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Enums
		static AstNode* ParseEnumName(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateEnumNameNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* enumHead = ParseEnumHead(data);
			if (enumHead->success)
			{
				Consume(data, TokenType::LEFT_BRACKET);
				// This is optional it's ok if it fails
				AstNode* enumeratorList = ParseEnumeratorList(data);
				if (enumeratorList->success)
				{
					// We don't really care about this, but we want to make sure to parse it if it's there
					bool trailingComma = Match(data, TokenType::COMMA);
				}
				Consume(data, TokenType::RIGHT_BRACKET);

				return GenerateEnumSpecifierNode(enumHead, enumeratorList);
			}

			FreeNode(enumHead);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumHead(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* enumKey = ParseEnumKey(data);
			if (!enumKey->success)
			{
				FreeNode(enumKey);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// This is optional
			AstNode* attributeSpecifierSequence = ParseAttributeSpecifierSequence(data);

			backtrackPosition = data.CurrentToken;
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			if (nestedNameSpecifier->success)
			{
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				// This is also optional
				AstNode* enumBase = ParseEnumBase(data);

				return GenerateEnumHeadNode(enumKey, attributeSpecifierSequence, nestedNameSpecifier, identifier, enumBase);
			}

			FreeNode(nestedNameSpecifier);
			BacktrackTo(data, backtrackPosition);

			Token identifier;
			identifier.m_Type = TokenType::None;
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
			}

			// enum base is optional so this is fine
			return GenerateEnumHeadNode(enumKey, attributeSpecifierSequence, GenerateNoSuccessAstNode(), identifier, ParseEnumBase(data));
		}

		static AstNode* ParseOpaqueEnumDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* enumKey = ParseEnumKey(data);
			if (!enumKey->success)
			{
				BacktrackTo(data, backtrackPosition);
				FreeNode(enumKey);
				return GenerateNoSuccessAstNode();
			}

			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
			AstNode* enumBase = ParseEnumBase(data);
			Consume(data, TokenType::SEMICOLON);

			return GenerateOpaqueEnumDeclNode(enumKey, attributeSpecifierSeq, identifier, enumBase);
		}

		static AstNode* ParseEnumKey(ParserData& data)
		{
			if (Match(data, TokenType::KW_ENUM))
			{
				if (Match(data, TokenType::KW_CLASS))
				{
					return GenerateEnumKeyNode(EnumKeyType::Class);
				}

				if (Match(data, TokenType::KW_STRUCT))
				{
					return GenerateEnumKeyNode(EnumKeyType::Struct);
				}

				return GenerateEnumKeyNode(EnumKeyType::Enum);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumBase(ParserData& data)
		{
			if (Match(data, TokenType::COLON))
			{
				AstNode* typeSpecifierSequence = ParseTypeSpecifierSequence(data);
				return GenerateEnumBaseNode(typeSpecifierSequence);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumeratorList(ParserData& data)
		{
			AstNode* result = ParseEnumeratorDefinition(data);

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateEnumeratorListNode(result, ParseEnumeratorList(data));
			}

			return GenerateEnumeratorListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseEnumeratorDefinition(ParserData& data)
		{
			Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);

			AstNode* constantExpression = Match(data, TokenType::EQUAL) ?
				ParseConstantExpression(data) :
				GenerateNoSuccessAstNode();

			return GenerateEnumeratorDefinitionNode(identifier, constantExpression);
		}

		// Namespaces
		static AstNode* ParseNamespaceName(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateNamespaceNameNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamespaceDefinition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* namedNamespaceDefinition = ParseNamedNamespaceDefinition(data);
			if (namedNamespaceDefinition->success)
			{
				return namedNamespaceDefinition;
			}
			FreeNode(namedNamespaceDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* unnamedNamespaceDefinition = ParseUnnamedNamespaceDefinition(data);
			if (unnamedNamespaceDefinition->success)
			{
				return unnamedNamespaceDefinition;
			}
			FreeNode(unnamedNamespaceDefinition);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamedNamespaceDefinition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			bool isInline = Match(data, TokenType::KW_INLINE);
			if (Match(data, TokenType::KW_NAMESPACE))
			{
				if (!(Peek(data) == TokenType::IDENTIFIER))
				{
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				Consume(data, TokenType::LEFT_CURLY_BRACKET);
				AstNode* namespaceBody = ParseNamespaceBody(data);
				Consume(data, TokenType::RIGHT_CURLY_BRACKET);

				return GenerateNamedNamespaceDefinitionNode(isInline, identifier, namespaceBody);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUnnamedNamespaceDefinition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			bool isInline = Match(data, TokenType::KW_INLINE);
			if (Match(data, TokenType::KW_NAMESPACE))
			{
				if (Peek(data) != TokenType::LEFT_BRACKET)
				{
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Consume(data, TokenType::LEFT_BRACKET);
				AstNode* namespaceBody = ParseNamespaceBody(data);
				Consume(data, TokenType::RIGHT_BRACKET);

				return GenerateUnnamedNamespaceDefinitionNode(isInline, namespaceBody);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamespaceBody(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* declarationSequence = ParseDeclarationSequence(data);
			if (declarationSequence->success)
			{
				return declarationSequence;
			}

			FreeNode(declarationSequence);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Namespace alias
		static AstNode* ParseNamespaceAliasDefinition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_NAMESPACE))
			{
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				if (!Match(data, TokenType::EQUAL))
				{
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				AstNode* qualifiedNamespaceSpecifier = ParseQualifiedNamespaceSpecifier(data);
				Consume(data, TokenType::SEMICOLON);

				return GenerateNamespaceAliasDefinitionNode(identifier, qualifiedNamespaceSpecifier);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseQualifiedNamespaceSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;

			bool isNested = Match(data, TokenType::COLON);
			if (isNested) Consume(data, TokenType::COLON);

			// This is optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			AstNode* namespaceName = ParseNamespaceName(data);
			if (namespaceName->success)
			{
				return GenerateQualifiedNamespaceSpecifierNode(isNested, nestedNameSpecifier, namespaceName);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(namespaceName);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Using
		static AstNode* ParseUsingDeclaration(ParserData& data)
		{
			if (Match(data, TokenType::KW_USING))
			{
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
					AstNode* unqualifiedId = ParseUnqualifiedId(data);
					Consume(data, TokenType::SEMICOLON);
					return GenerateUsingDeclarationNode(unqualifiedId);
				}

				bool isTypename = Match(data, TokenType::KW_TYPENAME);
				bool isNested = Match(data, TokenType::COLON);
				if (isNested) Consume(data, TokenType::COLON);
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				AstNode* unqualifiedId = ParseUnqualifiedId(data);
				Consume(data, TokenType::SEMICOLON);

				return GenerateUsingTypenameDeclarationNode(isTypename, isNested, nestedNameSpecifier, unqualifiedId);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUsingDirective(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			if (Match(data, TokenType::KW_USING))
			{
				bool isNested = Match(data, TokenType::COLON);
				if (isNested) Match(data, TokenType::COLON);

				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				AstNode* namespaceName = ParseNamespaceName(data);
				Consume(data, TokenType::SEMICOLON);

				return GenerateUsingDirectiveNode(attributeSpecifierSeq, isNested, nestedNameSpecifier, namespaceName);
			}

			FreeNode(attributeSpecifierSeq);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAsmDefinition(ParserData& data)
		{
			if (Match(data, TokenType::KW_ASM))
			{
				Consume(data, TokenType::LEFT_PAREN);
				Token stringLiteral = ConsumeCurrent(data, TokenType::STRING_LITERAL);
				Consume(data, TokenType::RIGHT_PAREN);
				Consume(data, TokenType::SEMICOLON);

				return GenerateAsmNode(stringLiteral);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLinkageSpecification(ParserData& data)
		{
			if (Match(data, TokenType::KW_EXTERN))
			{
				Token stringLiteral = ConsumeCurrent(data, TokenType::STRING_LITERAL);
				if (Match(data, TokenType::LEFT_BRACKET))
				{
					// Optional
					AstNode* declarationSeq = ParseDeclarationSequence(data);
					Consume(data, TokenType::RIGHT_BRACKET);

					return GenerateLinkageSpecificationBlockNode(stringLiteral, declarationSeq);
				}
				else
				{
					AstNode* declaration = ParseDeclaration(data);
					return GenerateLinkageSpecificationNode(stringLiteral, declaration);
				}
			}

			return GenerateNoSuccessAstNode();
		}

		// Attribute Specifiers
		static AstNode* ParseAttributeSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseAttributeSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextSpec = ParseAttributeSpecifier(data);
			result = GenerateAttributeSpecifierSequenceNode(result, nextSpec);

			return result;
		}

		static AstNode* ParseAttributeSpecifier(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_BRACKET))
			{
				Consume(data, TokenType::LEFT_BRACKET);
				AstNode* node = ParseAttributeList(data);
				Consume(data, TokenType::RIGHT_BRACKET);
				Consume(data, TokenType::RIGHT_BRACKET);
				return node;
			}

			return ParseAlignmentSpecifier(data);
		}

		static AstNode* ParseAlignmentSpecifier(ParserData& data)
		{
			if (Match(data, TokenType::KW_ALIGN_AS))
			{
				Consume(data, TokenType::LEFT_PAREN);
				int backtrackPosition = data.CurrentToken;
				AstNode* typeId = ParseTypeId(data);
				if (typeId->success)
				{
					bool hasElipsis = Match(data, TokenType::DOT);
					if (hasElipsis)
					{
						Consume(data, TokenType::DOT); Consume(data, TokenType::DOT);
					}
					Consume(data, TokenType::RIGHT_PAREN);

					return GenerateAlignAsTypeIdNode(typeId, hasElipsis);
				}
				FreeNode(typeId);
				BacktrackTo(data, backtrackPosition);

				AstNode* alignmentExpression = ParseAlignmentExpression(data);
				bool hasElipsis = Match(data, TokenType::DOT);
				if (hasElipsis)
				{
					Consume(data, TokenType::DOT); Consume(data, TokenType::DOT);
				}
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateAlignAsExpressionNode(alignmentExpression, hasElipsis);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseAttribute(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);

				return GenerateEmptyAttributeListNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateAttributeListNode(result, ParseAttributeList(data));
			}

			bool trailingComma = Match(data, TokenType::COMMA);
			bool elipsis = false;
			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT); Consume(data, TokenType::DOT);
				elipsis = true;
				if (trailingComma)
				{
					ErrorAtToken(data);
				}
			}

			return result;
		}

		static AstNode* ParseAttribute(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* attributeToken = ParseAttributeToken(data);
			if (!attributeToken->success)
			{
				FreeNode(attributeToken);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}
			// Optional
			AstNode* attributeArgumentClause = ParseAttributeArgumentClause(data);
			return GenerateAttributeNode(attributeToken, attributeArgumentClause);
		}

		static AstNode* ParseAttributeToken(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				Token id1 = ConsumeCurrent(data, TokenType::IDENTIFIER);
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					return GenerateAttributeTokenNode(id1, identifier);
				}

				Token empty;
				empty.m_Type = TokenType::None;
				return GenerateAttributeTokenNode(empty, id1);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeArgumentClause(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* balancedTokenSeq = ParseBalancedTokenSequence(data);
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateAttributeArgumentClauseNode(balancedTokenSeq);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBalancedTokenSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseBalancedToken(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextBalancedToken = ParseBalancedTokenSequence(data);
			result = GenerateBalancedTokenSeqNode(result, nextBalancedToken);

			return result;
		}

		static AstNode* ParseBalancedToken(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* result = ParseBalancedTokenSequence(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return result;
			}

			if (Match(data, TokenType::LEFT_BRACKET))
			{
				AstNode* result = ParseBalancedTokenSequence(data);
				Consume(data, TokenType::RIGHT_BRACKET);
				return result;
			}

			if (Match(data, TokenType::LEFT_CURLY_BRACKET))
			{
				AstNode* result = ParseBalancedTokenSequence(data);
				Consume(data, TokenType::RIGHT_CURLY_BRACKET);
				return result;
			}

			return GenerateBalancedTokenNode(ConsumeCurrent(data, Peek(data)));
		}

		// Declarations
		static AstNode* ParseInitDeclaratorList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseInitDeclarator(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclarator = ParseInitDeclaratorList(data);
			result = GenerateInitDeclaratorListNode(result, nextDeclarator);

			return result;
		}

		static AstNode* ParseInitDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* declarator = ParseDeclarator(data);
			if (!declarator->success)
			{
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* initializer = ParseInitializer(data);

			return GenerateInitDeclaratorNode(declarator, initializer);
		}

		static AstNode* ParseDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* noPtrDeclarator = ParseNoPtrDeclarator(data);
			if (noPtrDeclarator->success)
			{
				AstNode* parametersAndQualifiers = ParseParametersAndQualifiers(data);
				AstNode* trailingReturnType = ParseTrailingReturnType(data);
				if (parametersAndQualifiers->success && trailingReturnType->success)
				{
					return GenerateDeclaratorNode(noPtrDeclarator, parametersAndQualifiers, trailingReturnType);
				}
				FreeNode(parametersAndQualifiers);
				FreeNode(trailingReturnType);
			}
			FreeNode(noPtrDeclarator);
			BacktrackTo(data, backtrackPosition);

			AstNode* ptrDeclarator = ParsePtrDeclarator(data);
			if (ptrDeclarator->success)
			{
				return ptrDeclarator;
			}

			FreeNode(ptrDeclarator);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* noPtrDeclarator = ParseNoPtrDeclarator(data);
			if (noPtrDeclarator->success)
			{
				return noPtrDeclarator;
			}
			FreeNode(noPtrDeclarator);
			BacktrackTo(data, backtrackPosition);

			AstNode* ptrOperator = ParsePtrOperator(data);
			if (ptrOperator->success)
			{
				AstNode* ptrDeclarator = ParsePtrDeclarator(data);
				if (ptrDeclarator->success)
				{
					return GeneratePtrDeclaratorNode(ptrOperator, ptrDeclarator);
				}
				FreeNode(ptrDeclarator);
			}

			FreeNode(ptrOperator);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNoPtrDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers(data);
			if (parametersAndQualifiers->success)
			{
				AstNode* noptrDeclarator = ParseNoPtrDeclarator(data);
				return GenerateNoPtrParamAndQualDeclaratorNode(parametersAndQualifiers, noptrDeclarator);
			}
			FreeNode(parametersAndQualifiers);
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* ptrDeclarator = ParsePtrDeclarator(data);
				if (ptrDeclarator->success)
				{
					return GenerateNoPtrParenDeclaratorNode(ptrDeclarator);
				}
				FreeNode(ptrDeclarator);
				BacktrackTo(data, backtrackPosition);
			}

			if (Match(data, TokenType::LEFT_BRACKET))
			{
				AstNode* constantExpression = ParseConstantExpression(data);
				Consume(data, TokenType::RIGHT_BRACKET);
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				AstNode* noptrDeclarator = ParseNoPtrDeclarator(data);
				return GenerateNoPtrBracketDeclaratorNode(constantExpression, attributeSpecifierSeq, noptrDeclarator);
			}

			AstNode* declaratorId = ParseDeclaratorId(data);
			if (declaratorId->success)
			{
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				return GenerateNoPtrDeclaratorNode(declaratorId, attributeSpecifierSeq);
			}
			FreeNode(declaratorId);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseParametersAndQualifiers(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause(data);
				Consume(data, TokenType::RIGHT_PAREN);
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				// Optional
				AstNode* cvQualifierSeq = ParseCvQualifierSequence(data);
				// Optional
				AstNode* refQualifier = ParseRefQualifier(data);
				// Optional
				AstNode* exceptionSpec = ParseExceptionSpecification(data);

				return GenerateParametersAndQualifiersNode(parameterDeclarationClause, attributeSpecifierSeq, cvQualifierSeq, refQualifier, exceptionSpec);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTrailingReturnType(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::ARROW))
			{
				AstNode* trailingTypeSpecifierSeq = ParseTrailingTypeSpecifierSequence(data);
				if (!trailingTypeSpecifierSeq->success)
				{
					FreeNode(trailingTypeSpecifierSeq);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				// Optional
				AstNode* abstractDeclarator = ParseAbstractDeclarator(data);

				return GenerateTrailingReturnTypeNode(trailingTypeSpecifierSeq, abstractDeclarator);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrOperator(ParserData& data)
		{
			if (Match(data, TokenType::STAR))
			{
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				// Optional
				AstNode* cvQualifierSeq = ParseCvQualifierSequence(data);
				return GeneratePtrStarNode(attributeSpecifierSeq, cvQualifierSeq);
			}

			if (Match(data, TokenType::AND))
			{
				if (Match(data, TokenType::AND))
				{
					return GenerateRefRefNode(ParseAttributeSpecifierSequence(data));
				}

				return GenerateRefNode(ParseAttributeSpecifierSequence(data));
			}

			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				if (!nestedNameSpecifier)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Consume(data, TokenType::STAR);
				// Both optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				AstNode* cvQualifierSeq = ParseCvQualifierSequence(data);
				return GeneratePtrNamespaceStarNode(nestedNameSpecifier, attributeSpecifierSeq, cvQualifierSeq);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCvQualifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseCvQualifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextQualifier = ParseCvQualifierSequence(data);
				result = GenerateCvQualifierSeqNode(result, nextQualifier);
				if (!nextQualifier->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseCvQualifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_CONST || Peek(data) == TokenType::KW_VOLATILE)
			{
				Token token = ConsumeCurrent(data, Peek(data));
				return GenerateCvQualifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseRefQualifier(ParserData& data)
		{
			if (Match(data, TokenType::AND))
			{
				bool doubleRef = Match(data, TokenType::AND);
				return GenerateRefQualifierNode(doubleRef);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclaratorId(ParserData& data)
		{
			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
				return ParseIdExpression(data);
			}

			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				AstNode* className = ParseClassName(data);
				if (!className->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(className);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateDeclaratorIdNode(nestedNameSpecifier, className);
			}

			backtrackPosition = data.CurrentToken;
			AstNode* idExpression = ParseIdExpression(data);
			if (idExpression->success)
			{
				return idExpression;
			}
			FreeNode(idExpression);
			BacktrackTo(data, backtrackPosition);

			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			AstNode* className = ParseClassName(data);
			if (className->success)
			{
				return GenerateDeclaratorIdNode(nestedNameSpecifier, className);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(className);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// dcl.name
		static AstNode* ParseTypeId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* typeSpecifierSequence = ParseTypeSpecifierSequence(data);
			if (!typeSpecifierSequence->success)
			{
				FreeNode(typeSpecifierSequence);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* abstractDeclarator = ParseAbstractDeclarator(data);
			return GenerateTypeIdNode(typeSpecifierSequence, abstractDeclarator);
		}

		static AstNode* ParseAbstractDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::DOT))
			{
				if (Match(data, TokenType::DOT))
				{
					if (Match(data, TokenType::DOT))
					{
						return GenerateAbstractElipsisDeclaratorNode();
					}
				}
			}
			BacktrackTo(data, backtrackPosition);

			// Optional
			AstNode* noptrAbstractDeclarator = ParseNoptrAbstractDeclarator(data);
			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers(data);
			if (parametersAndQualifiers->success)
			{
				AstNode* trailingReturnType = ParseTrailingReturnType(data);
				if (trailingReturnType->success)
				{
					return GenerateAbstractDeclaratorNode(noptrAbstractDeclarator, parametersAndQualifiers, trailingReturnType);
				}
				FreeNode(trailingReturnType);
			}
			FreeNode(noptrAbstractDeclarator);
			FreeNode(parametersAndQualifiers);
			BacktrackTo(data, backtrackPosition);

			AstNode* ptrAbstractDeclarator = ParsePtrAbstractDeclarator(data);
			if (ptrAbstractDeclarator->success)
			{
				return ptrAbstractDeclarator;
			}

			FreeNode(ptrAbstractDeclarator);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrAbstractDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* ptrOperator = ParsePtrOperator(data);
			if (ptrOperator->success)
			{
				// Optional
				AstNode* ptrAbstractDeclarator = ParsePtrAbstractDeclarator(data);
				return GeneratePtrAbstractDeclaratorNode(ptrOperator, ptrAbstractDeclarator);
			}
			FreeNode(ptrOperator);
			BacktrackTo(data, backtrackPosition);

			return ParseNoptrAbstractDeclarator(data);
		}

		static AstNode* ParseNoptrAbstractDeclarator(ParserData& data)
		{
			// TODO: Not sure if this will work right...?
			int backtrackPosition = data.CurrentToken;
			AstNode* ptrAbstractDeclarator = GenerateNoSuccessAstNode();
			if (Match(data, TokenType::LEFT_PAREN))
			{
				ptrAbstractDeclarator = ParsePtrAbstractDeclarator(data);
				if (!ptrAbstractDeclarator->success)
				{
					FreeNode(ptrAbstractDeclarator);
					BacktrackTo(data, backtrackPosition);
				}
				Consume(data, TokenType::RIGHT_PAREN);
			}

			if (Match(data, TokenType::LEFT_BRACKET))
			{
				AstNode* constantExpression = ParseConstantExpression(data);
				if (!constantExpression->success)
				{
					FreeNode(constantExpression);
					FreeNode(ptrAbstractDeclarator);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
				return GenerateNoptrAbstractExpressionDeclaratorNode(ptrAbstractDeclarator, constantExpression, attributeSpecifierSeq, ParseNoptrAbstractDeclarator(data));
			}

			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers(data);
			if (parametersAndQualifiers->success)
			{
				return GenerateNoptrAbstractDeclaratorNode(ptrAbstractDeclarator, parametersAndQualifiers, ParseNoptrAbstractDeclarator(data));
			}

			FreeNode(parametersAndQualifiers);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// dcl.fct
		static AstNode* ParseParameterDeclarationClause(ParserData& data)
		{
			// Optional
			AstNode* parameterDeclarationList = ParseParameterDeclarationList(data);
			if (parameterDeclarationList->success)
			{
				if (Match(data, TokenType::COMMA))
				{
					if (Match(data, TokenType::DOT))
					{
						Consume(data, TokenType::DOT);
						Consume(data, TokenType::DOT);
					}
				}
				return parameterDeclarationList;
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return parameterDeclarationList;
		}

		static AstNode* ParseParameterDeclarationList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseParameterDeclaration(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				AstNode* nextParameter = ParseParameterDeclarationList(data);
				result = GenerateParameterDeclarationListNode(result, nextParameter);
			}

			return result;
		}

		static AstNode* ParseParameterDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence(data);
			if (declSpecifierSeq->success)
			{
				int backtrackPosition2 = data.CurrentToken;
				AstNode* declarator = ParseDeclarator(data);
				if (declarator->success)
				{
					if (Match(data, TokenType::EQUAL))
					{
						AstNode* initializerClause = ParseInitializerClause(data);
						if (!initializerClause->success)
						{
							FreeNode(initializerClause);
							FreeNode(declarator);
							FreeNode(attributeSpecifierSeq);
							FreeNode(declSpecifierSeq);
							BacktrackTo(data, backtrackPosition);
							return GenerateNoSuccessAstNode();
						}

						return GenerateParameterDefaultDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, declarator, initializerClause);
					}

					return GenerateParameterDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, declarator);
				}
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition2);

				// Optional
				AstNode* abstractDeclarator = ParseAbstractDeclarator(data);
				if (Match(data, TokenType::EQUAL))
				{
					AstNode* initializerClause = ParseInitializerClause(data);
					if (!initializerClause->success)
					{
						FreeNode(initializerClause);
						FreeNode(abstractDeclarator);
						FreeNode(attributeSpecifierSeq);
						FreeNode(declSpecifierSeq);
						BacktrackTo(data, backtrackPosition);
						return GenerateNoSuccessAstNode();
					}

					return GenerateParameterAbstractDefaultDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, abstractDeclarator, initializerClause);
				}

				return GenerateParameterAbstractDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, abstractDeclarator);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Functions
		static AstNode* ParseFunctionDefinition(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence(data);
			AstNode* declarator = ParseDeclarator(data);
			if (!declarator->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(data, TokenType::EQUAL))
			{
				if (Match(data, TokenType::KW_DEFAULT))
				{
					Consume(data, TokenType::SEMICOLON);
					return GenerateFunctionDefaultDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, AutoFunctionType::Default);
				}

				if (Match(data, TokenType::KW_DELETE))
				{
					Consume(data, TokenType::SEMICOLON);
					return GenerateFunctionDefaultDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, AutoFunctionType::Delete);
				}

				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* functionBody = ParseFunctionBody(data);
			if (functionBody->success)
			{
				return GenerateFunctionDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, functionBody);
			}

			FreeNode(functionBody);
			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			FreeNode(declarator);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionBody(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* functionTryBlock = ParseFunctionTryBlock(data);
			if (functionTryBlock->success)
			{
				return functionTryBlock;
			}
			FreeNode(functionTryBlock);
			BacktrackTo(data, backtrackPosition);

			// Optional
			AstNode* ctorInitializer = ParseCtorInitializer(data);
			AstNode* compoundStatement = ParseCompoundStatement(data);
			if (compoundStatement->success)
			{
				return GenerateFunctionBodyNode(ctorInitializer, compoundStatement);
			}

			FreeNode(ctorInitializer);
			FreeNode(compoundStatement);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Init
		static AstNode* ParseInitializer(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_PAREN))
			{
				AstNode* expressionList = ParseExpressionList(data);
				Consume(data, TokenType::RIGHT_PAREN);
				return expressionList;
			}

			return ParseBraceOrEqualInitializer(data);
		}

		static AstNode* ParseBraceOrEqualInitializer(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::EQUAL))
			{
				AstNode* result = ParseInitializerClause(data);
				if (result->success)
				{
					return result;
				}
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
			}

			return ParseBracedInitList(data);
		}

		static AstNode* ParseInitializerClause(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* bracedInitList = ParseBracedInitList(data);
			if (bracedInitList->success)
			{
				return bracedInitList;
			}
			FreeNode(bracedInitList);
			BacktrackTo(data, backtrackPosition);

			AstNode* assignmentExpression = ParseAssignmentExpression(data);
			if (assignmentExpression->success)
			{
				return assignmentExpression;
			}
			FreeNode(assignmentExpression);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseInitializerList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseInitializerClause(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextInitList = nullptr;
			if (Match(data, TokenType::COMMA))
			{
				nextInitList = ParseInitializerList(data);
			}
			else
			{
				nextInitList = GenerateNoSuccessAstNode();
			}

			bool hasElipsis = Match(data, TokenType::DOT);
			if (hasElipsis)
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return GenerateInitializerListNode(result, nextInitList);
		}

		static AstNode* ParseBracedInitList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::LEFT_CURLY_BRACKET))
			{
				AstNode* initializerList = ParseInitializerList(data);
				if (initializerList->success)
				{
					bool trailingComma = Match(data, TokenType::COMMA);
					if (Match(data, TokenType::RIGHT_CURLY_BRACKET))
					{
						return GenerateBracedInitListNode(initializerList);
					}
				}
				FreeNode(initializerList);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Classes
		static AstNode* ParseClassName(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateClassNameNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return ParseSimpleTemplateId(data);
		}

		static AstNode* ParseClassSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* classHead = ParseClassHead(data);
			if (!classHead->success)
			{
				FreeNode(classHead);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			Consume(data, TokenType::LEFT_CURLY_BRACKET);
			// Optional
			AstNode* memberSpecification = ParseMemberSpecification(data);
			Consume(data, TokenType::RIGHT_CURLY_BRACKET);

			return GenerateClassSpecifierNode(classHead, memberSpecification);
		}

		static AstNode* ParseClassHead(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* classKey = ParseClassKey(data);
			if (!classKey->success)
			{
				FreeNode(classKey);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);

			backtrackPosition = data.CurrentToken;
			AstNode* classHeadName = ParseClassHeadName(data);
			if (classHeadName->success)
			{
				// Optional
				AstNode* classVirtSpecifierSeq = ParseClassVirtSpecifierSequence(data);
				// Optional
				AstNode* baseClause = ParseBaseClause(data);
				return GenerateClassVirtualHeadNode(classKey, attributeSpecifierSeq, classHeadName, classVirtSpecifierSeq, baseClause);
			}
			FreeNode(classHeadName);
			BacktrackTo(data, backtrackPosition);

			// Optional
			AstNode* baseClause = ParseBaseClause(data);
			return GenerateClassHeadNode(classKey, attributeSpecifierSeq, baseClause);
		}

		static AstNode* ParseClassHeadName(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				// Make sure there's a chance of a nested name before blindly consuming it
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}

			AstNode* className = ParseClassName(data);
			if (!className->success)
			{
				FreeNode(nestedNameSpecifier);
				FreeNode(className);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateClassHeadNameNode(nestedNameSpecifier, className);
		}

		static AstNode* ParseClassVirtSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseClassVirtSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextSpec = ParseClassVirtSpecifierSequence(data);
				result = GenerateClassVirtSpecifierSeqNode(result, nextSpec);
				if (!nextSpec->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseClassVirtSpecifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_FINAL || Peek(data) == TokenType::KW_EXPLICIT)
			{
				Token token = ConsumeCurrent(data, Peek(data));
				return GenerateClassVirtSpecifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseClassKey(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_CLASS || Peek(data) == TokenType::KW_STRUCT || Peek(data) == TokenType::KW_UNION)
			{
				Token token = ConsumeCurrent(data, Peek(data));
				return GenerateClassKeyNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		// Class Members
		static AstNode* ParseMemberSpecification(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* accessSpecifier = ParseAccessSpecifier(data);
			if (accessSpecifier->success)
			{
				Consume(data, TokenType::COLON);
				// Optional
				AstNode* memberSpecification = ParseMemberSpecification(data);
				return GenerateMemberAndAccessSpecifierNode(accessSpecifier, memberSpecification);
			}
			FreeNode(accessSpecifier);
			BacktrackTo(data, backtrackPosition);

			AstNode* memberDeclaration = ParseMemberDeclaration(data);
			if (memberDeclaration->success)
			{
				// Optional
				AstNode* memberSpecification = ParseMemberSpecification(data);
				return GenerateMemberSpecifierNode(memberDeclaration, memberSpecification);
			}

			FreeNode(memberDeclaration);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseMemberDeclaration(ParserData& data)
		{
			// TODO: Does this really work right...?
			int backtrackPosition = data.CurrentToken;
			AstNode* functionDefinition = ParseFunctionDefinition(data);
			if (functionDefinition->success)
			{
				bool trailingSemicolon = Match(data, TokenType::SEMICOLON);
				return GenerateMemberFunctionDeclarationNode(functionDefinition, trailingSemicolon);
			}
			FreeNode(functionDefinition);
			BacktrackTo(data, backtrackPosition);

			AstNode* usingDeclaration = ParseUsingDeclaration(data);
			if (usingDeclaration->success)
			{
				return usingDeclaration;
			}
			FreeNode(usingDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* staticAssertDeclaration = ParseStaticAssertDeclaration(data);
			if (staticAssertDeclaration->success)
			{
				return staticAssertDeclaration;
			}
			FreeNode(staticAssertDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* templateDeclaration = ParseTemplateDeclaration(data);
			if (templateDeclaration->success)
			{
				return templateDeclaration;
			}
			FreeNode(templateDeclaration);
			BacktrackTo(data, backtrackPosition);

			AstNode* aliasDeclaration = ParseAliasDeclaration(data);
			if (aliasDeclaration->success)
			{
				return aliasDeclaration;
			}
			FreeNode(aliasDeclaration);
			BacktrackTo(data, backtrackPosition);

			// All optional except semicolon
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence(data);
			AstNode* memberDeclaratorList = ParseMemberDeclaratorList(data);
			if (!Match(data, TokenType::SEMICOLON))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(memberDeclaratorList);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemberDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, memberDeclaratorList);
		}

		static AstNode* ParseMemberDeclaratorList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseMemberDeclarator(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextDeclarator = ParseMemberDeclaratorList(data);
				result = GenerateMemberDeclaratorListNode(result, nextDeclarator);
				if (!nextDeclarator->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseMemberDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* declarator = ParseDeclarator(data);
			if (declarator->success)
			{
				// Optional
				AstNode* virtSpecifierSeq = ParseVirtSpecifierSequence(data);
				// Also optional, but there's a chance we could be referring to a different node
				int backtrackPosition2 = data.CurrentToken;
				AstNode* pureSpecifier = ParsePureSpecifier(data);
				if (pureSpecifier->success)
				{
					return GenerateMemberDeclaratorPureNode(declarator, virtSpecifierSeq, pureSpecifier);
				}
				FreeNode(pureSpecifier);
				BacktrackTo(data, backtrackPosition2);
				// Also optional, but this is the fallback if neither succeeds
				AstNode* braceOrEqualInitializer = ParseBraceOrEqualInitializer(data);
				return GenerateMemberDeclaratorBraceNode(declarator, virtSpecifierSeq, braceOrEqualInitializer);
			}
			FreeNode(declarator);
			BacktrackTo(data, backtrackPosition);

			// Optional
			Token identifier;
			identifier.m_Type = TokenType::None;
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
			}

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			// Optional
			AstNode* virtSpecifierSeq = ParseVirtSpecifierSequence(data);
			if (!Match(data, TokenType::COLON))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(virtSpecifierSeq);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* constantExpression = ParseConstantExpression(data);
			if (!constantExpression->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(virtSpecifierSeq);
				FreeNode(constantExpression);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemberDeclaratorNode(identifier, attributeSpecifierSeq, virtSpecifierSeq, constantExpression);
		}

		static AstNode* ParseVirtSpecifierSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseVirtSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextSpec = ParseVirtSpecifier(data);
				result = GenerateVirtSpecifierSeqNode(result, nextSpec);
				if (!nextSpec->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseVirtSpecifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_OVERRIDE || Peek(data) == TokenType::KW_FINAL || Peek(data) == TokenType::KW_NEW)
			{
				Token token = ConsumeCurrent(data, Peek(data));
				return GenerateVirtSpecifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePureSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::EQUAL))
			{
				if (Peek(data) == TokenType::INTEGER_LITERAL)
				{
					Token token = ConsumeCurrent(data, TokenType::INTEGER_LITERAL);
					if (strcmp(token.m_Lexeme, "0") == 0)
					{
						return GeneratePureSpecifierNode();
					}
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Derived classes
		static AstNode* ParseBaseClause(ParserData& data)
		{
			if (Match(data, TokenType::COLON))
			{
				return ParseBaseSpecifierList(data);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBaseSpecifierList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseBaseSpecifier(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				AstNode* nextBase = ParseBaseSpecifierList(data);
				result = GenerateBaseSpecifierListNode(result, nextBase);
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return result;
		}

		static AstNode* ParseBaseSpecifier(ParserData& data)
		{
			// TODO: this is weird, make sure I didn't goof up here
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			bool isVirtual = Match(data, TokenType::KW_VIRTUAL);

			int backtrackPosition2 = data.CurrentToken;
			AstNode* accessSpecifier = ParseAccessSpecifier(data);
			if (!accessSpecifier->success)
			{
				FreeNode(accessSpecifier);
				BacktrackTo(data, backtrackPosition2);
				accessSpecifier = GenerateNoSuccessAstNode();

				if (!isVirtual)
				{
					isVirtual = Match(data, TokenType::KW_VIRTUAL);
				}
			}

			AstNode* baseTypeSpecifier = ParseBaseTypeSpecifier(data);
			if (baseTypeSpecifier->success)
			{
				return GenerateBaseSpecifierNode(attributeSpecifierSeq, isVirtual, accessSpecifier, baseTypeSpecifier);
			}

			FreeNode(baseTypeSpecifier);
			FreeNode(accessSpecifier);
			FreeNode(attributeSpecifierSeq);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseClassOrDecltype(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* decltypeSpecifier = ParseDecltypeSpecifier(data);
			if (decltypeSpecifier->success)
			{
				return decltypeSpecifier;
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::COLON))
			{
				Consume(data, TokenType::COLON);
			}

			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier(data);
			}
			AstNode* className = ParseClassName(data);
			if (className->success)
			{
				return GenerateClassOrDecltypeNode(nestedNameSpecifier, className);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(className);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBaseTypeSpecifier(ParserData& data)
		{
			return ParseClassOrDecltype(data);
		}

		static AstNode* ParseAccessSpecifier(ParserData& data)
		{
			if (Peek(data) == TokenType::KW_PRIVATE || Peek(data) == TokenType::KW_PROTECTED || Peek(data) == TokenType::KW_PUBLIC)
			{
				Token accessSpecifier = ConsumeCurrent(data, Peek(data));
				return GenerateAccessSpecifierNode(accessSpecifier);
			}

			return GenerateNoSuccessAstNode();
		}

		// Class conversion functions
		static AstNode* ParseConversionFunctionId(ParserData& data)
		{
			if (Match(data, TokenType::KW_OPERATOR))
			{
				AstNode* conversionTypeId = ParseConversionTypeId(data);
				return GenerateConversionFunctionIdNode(conversionTypeId);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseConversionTypeId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence(data);
			if (!typeSpecifierSeq->success)
			{
				FreeNode(typeSpecifierSeq);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* conversionDeclarator = ParseConversionDeclarator(data);
			return GenerateConversionTypeIdNode(typeSpecifierSeq, conversionDeclarator);
		}

		static AstNode* ParseConversionDeclarator(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* ptrOperator = ParsePtrOperator(data);
			if (!ptrOperator->success)
			{
				FreeNode(ptrOperator);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* conversionDeclarator = ParseConversionDeclarator(data);
			return GenerateConversionDeclaratorNode(ptrOperator, conversionDeclarator);
		}

		// Class initializers
		static AstNode* ParseCtorInitializer(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::COLON))
			{
				AstNode* memInitializerList = ParseMemInitializerList(data);
				if (memInitializerList->success)
				{
					return memInitializerList;
				}
				FreeNode(memInitializerList);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseMemInitializerList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseMemInitializer(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				AstNode* nextInitializer = ParseMemInitializer(data);
				result = GenerateMemInitializerListNode(result, nextInitializer);
				if (!nextInitializer->success)
				{
					break;
				}
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return result;
		}

		static AstNode* ParseMemInitializer(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* memInitializerId = ParseMemInitializerId(data);
			if (!memInitializerId->success)
			{
				FreeNode(memInitializerId);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(data, TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList(data);
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateMemExpressionInitializerNode(memInitializerId, expressionList);
			}

			AstNode* bracedInitList = ParseBracedInitList(data);
			if (!bracedInitList->success)
			{
				FreeNode(memInitializerId);
				FreeNode(bracedInitList);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemBracedInitializerNode(memInitializerId, bracedInitList);
		}

		static AstNode* ParseMemInitializerId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* classOrDecltype = ParseClassOrDecltype(data);
			if (classOrDecltype->success)
			{
				return classOrDecltype;
			}
			FreeNode(classOrDecltype);
			BacktrackTo(data, backtrackPosition);

			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateMemInitializerIdNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		// Operator overloading
		static OverloadableOperatorType ParseOverloadableOperator(ParserData& data)
		{
			if (Match(data, TokenType::KW_NEW))
			{
				if (Match(data, TokenType::LEFT_BRACKET))
				{
					Consume(data, TokenType::RIGHT_BRACKET);
					return OverloadableOperatorType::NewArr;
				}
				return  OverloadableOperatorType::New;
			}

			if (Match(data, TokenType::KW_DELETE))
			{
				if (Match(data, TokenType::LEFT_BRACKET))
				{
					Consume(data, TokenType::RIGHT_BRACKET);
					return OverloadableOperatorType::DeleteArr;
				}
				return OverloadableOperatorType::Delete;
			}

			Token token = ConsumeCurrent(data, Peek(data));
			switch (token.m_Type)
			{
			case TokenType::PLUS:
				return OverloadableOperatorType::Plus;
			case TokenType::MINUS:
				return OverloadableOperatorType::Minus;
			case TokenType::STAR:
				return OverloadableOperatorType::Multiply;
			case TokenType::DIV:
				return OverloadableOperatorType::Divide;
			case TokenType::MODULO:
				return OverloadableOperatorType::Modulo;
			case TokenType::CARET:
				return OverloadableOperatorType::Xor;
			case TokenType::AND:
				return OverloadableOperatorType::BitAnd;
			case TokenType::PIPE:
				return OverloadableOperatorType::BitOr;
			case TokenType::TILDE:
				return OverloadableOperatorType::BitComplement;
			case TokenType::BANG:
				return OverloadableOperatorType::Not;
			case TokenType::EQUAL:
				return OverloadableOperatorType::Assign;
			case TokenType::LEFT_ANGLE_BRACKET:
				return OverloadableOperatorType::LessThan;
			case TokenType::RIGHT_ANGLE_BRACKET:
				return OverloadableOperatorType::GreaterThan;
			case TokenType::PLUS_EQUAL:
				return OverloadableOperatorType::PlusEqual;
			case TokenType::MINUS_EQUAL:
				return OverloadableOperatorType::MinusEqual;
			case TokenType::STAR_EQUAL:
				return OverloadableOperatorType::MultiplyEqual;
			case TokenType::DIV_EQUAL:
				return OverloadableOperatorType::DivideEqual;
			case TokenType::MODULO_EQUAL:
				return OverloadableOperatorType::ModuloEqual;
			case TokenType::CARET_EQUAL:
				return OverloadableOperatorType::CaretEqual;
			case TokenType::AND_EQUAL:
				return OverloadableOperatorType::BitAndEqual;
			case TokenType::PIPE_EQUAL:
				return OverloadableOperatorType::BitOrEqual;
			case TokenType::LEFT_SHIFT:
				return OverloadableOperatorType::LeftShift;
			case TokenType::RIGHT_SHIFT:
				return OverloadableOperatorType::RightShift;
			case TokenType::RIGHT_SHIFT_EQUAL:
				return OverloadableOperatorType::RightShiftEqual;
			case TokenType::LEFT_SHIFT_EQUAL:
				return OverloadableOperatorType::LeftShiftEqual;
			case TokenType::EQUAL_EQUAL:
				return OverloadableOperatorType::EqualEqual;
			case TokenType::BANG_EQUAL:
				return OverloadableOperatorType::NotEqual;
			case TokenType::LESS_THAN_EQ:
				return OverloadableOperatorType::LessThanEqual;
			case TokenType::GREATER_THAN_EQ:
				return OverloadableOperatorType::GreaterThanEqual;
			case TokenType::LOGICAL_AND:
				return OverloadableOperatorType::LogicAnd;
			case TokenType::LOGICAL_OR:
				return OverloadableOperatorType::LogicOr;
			case TokenType::PLUS_PLUS:
				return OverloadableOperatorType::PlusPlus;
			case TokenType::MINUS_MINUS:
				return OverloadableOperatorType::MinusMinus;
			case TokenType::COMMA:
				return OverloadableOperatorType::Comma;
			case TokenType::ARROW:
			{
				if (Match(data, TokenType::STAR))
				{
					return OverloadableOperatorType::ArrowStar;
				}
				return OverloadableOperatorType::Arrow;
			}
			case TokenType::LEFT_PAREN:
				Consume(data, TokenType::RIGHT_PAREN);
				return OverloadableOperatorType::ParenGroup;
			case TokenType::LEFT_BRACKET:
				Consume(data, TokenType::RIGHT_BRACKET);
				return OverloadableOperatorType::BracketGroup;
			}

			return OverloadableOperatorType::None;
		}

		static AstNode* ParseOperatorFunctionId(ParserData& data)
		{
			if (Match(data, TokenType::KW_OPERATOR))
			{
				OverloadableOperatorType op = ParseOverloadableOperator(data);
				if (Match(data, TokenType::LEFT_ANGLE_BRACKET))
				{
					AstNode* templateArgList = ParseTemplateArgumentList(data);
					Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
					return GenerateOperatorFunctionIdNode(op, templateArgList);
				}

				return GenerateOperatorFunctionIdNode(op, GenerateNoSuccessAstNode());
			}

			return GenerateNoSuccessAstNode();
		}

		// Literal overrides
		static AstNode* ParseLiteralOperatorId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_OPERATOR))
			{
				if (Peek(data) == TokenType::STRING_LITERAL)
				{
					Token token = ConsumeCurrent(data, TokenType::STRING_LITERAL);
					Logger::Assert(ParserString::StringLength(token.m_Lexeme) == 0, "Invalid custom overloaded operator. Syntax is 'operator\"\" identifier'");
					if (Peek(data) == TokenType::IDENTIFIER)
					{
						Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
						return GenerateLiteralOperatorIdNode(identifier);
					}
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Templates
		static AstNode* ParseTemplateDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				AstNode* templateParameterList = ParseTemplateParameterList(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				AstNode* declaration = ParseDeclaration(data);

				if (!(templateParameterList->success && declaration->success))
				{
					FreeNode(templateParameterList);
					FreeNode(declaration);
					BacktrackTo(data, backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateTemplateDeclarationNode(templateParameterList, declaration);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateParameterList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseTemplateParameter(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateTemplateParameterListNode(result, ParseTemplateParameterList(data));
			}

			return GenerateTemplateParameterListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseTemplateParameter(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* typeParameter = ParseTypeParameter(data);
			if (typeParameter->success)
			{
				return typeParameter;
			}
			FreeNode(typeParameter);
			BacktrackTo(data, backtrackPosition);

			AstNode* parameterDeclaration = ParseParameterDeclaration(data);
			if (parameterDeclaration->success)
			{
				return parameterDeclaration;
			}

			FreeNode(parameterDeclaration);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeParameter(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				if (Match(data, TokenType::LEFT_ANGLE_BRACKET))
				{
					AstNode* templateParameterList = ParseTemplateParameterList(data);
					if (templateParameterList->success)
					{
						if (Match(data, TokenType::KW_CLASS))
						{
							bool hasElipsis = Match(data, TokenType::DOT);
							if (hasElipsis)
							{
								Consume(data, TokenType::DOT);
								Consume(data, TokenType::DOT);
							}

							Token identifier;
							identifier.m_Type = TokenType::None;
							if (Peek(data) == TokenType::IDENTIFIER)
							{
								identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
							}

							if (Match(data, TokenType::EQUAL))
							{
								return GenerateTypeTemplateParameterNode(templateParameterList, identifier, ParseIdExpression(data));
							}
							return GenerateTypeTemplateParameterNode(templateParameterList, identifier, GenerateNoSuccessAstNode());
						}
					}
					FreeNode(templateParameterList);
				}
			}
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::KW_TYPENAME))
			{
				bool hasElipsis = Match(data, TokenType::DOT);
				if (hasElipsis)
				{
					Consume(data, TokenType::DOT);
					Consume(data, TokenType::DOT);
				}

				Token identifier;
				identifier.m_Type = TokenType::None;
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				}

				if (Match(data, TokenType::EQUAL))
				{
					return GenerateTypeTypenameParameterNode(identifier, ParseTypeId(data));
				}
				return GenerateTypeTypenameParameterNode(identifier, GenerateNoSuccessAstNode());
			}
			BacktrackTo(data, backtrackPosition);

			if (Match(data, TokenType::KW_CLASS))
			{
				bool hasElipsis = Match(data, TokenType::DOT);
				if (hasElipsis)
				{
					Consume(data, TokenType::DOT);
					Consume(data, TokenType::DOT);
				}

				Token identifier;
				identifier.m_Type = TokenType::None;
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				}

				if (Match(data, TokenType::EQUAL))
				{
					return GenerateTypeClassParameterNode(identifier, ParseTypeId(data));
				}
				return GenerateTypeClassParameterNode(identifier, GenerateNoSuccessAstNode());
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseSimpleTemplateId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* templateName = ParseTemplateName(data);
			if (templateName->success)
			{
				if (Match(data, TokenType::LEFT_ANGLE_BRACKET))
				{
					// Optional
					AstNode* templateArgumentList = ParseTemplateArgumentList(data);
					if (Match(data, TokenType::RIGHT_ANGLE_BRACKET))
					{
						return GenerateSimpleTemplateIdNode(templateName, templateArgumentList);
					}
				}
			}

			FreeNode(templateName);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateId(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
			if (simpleTemplateId->success)
			{
				return simpleTemplateId;
			}
			FreeNode(simpleTemplateId);
			BacktrackTo(data, backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId(data);
			if (literalOperatorId->success)
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				// Optional
				AstNode* templateArgumentList = ParseTemplateArgumentList(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				return GenerateLiteralOperatorTemplateIdNode(literalOperatorId, templateArgumentList);
			}
			FreeNode(literalOperatorId);
			BacktrackTo(data, backtrackPosition);

			AstNode* operatorFunctionId = ParseOperatorFunctionId(data);
			if (operatorFunctionId->success)
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				// Optional
				AstNode* templateArgumentList = ParseTemplateArgumentList(data);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				return GenerateFunctionOperatorTemplateIdNode(operatorFunctionId, templateArgumentList);
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateName(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateTemplateNameNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateArgumentList(ParserData& data)
		{
			AstNode* result = ParseTemplateArgument(data);

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateTemplateArgumentListNode(result, ParseTemplateArgumentList(data));
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return GenerateTemplateArgumentListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseTemplateArgument(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* idExpression = ParseIdExpression(data);
			if (idExpression->success)
			{
				return idExpression;
			}
			FreeNode(idExpression);
			BacktrackTo(data, backtrackPosition);

			AstNode* typeId = ParseTypeId(data);
			if (typeId->success)
			{
				return typeId;
			}
			FreeNode(typeId);
			BacktrackTo(data, backtrackPosition);

			AstNode* constantExpression = ParseConstantExpression(data);
			if (constantExpression->success)
			{
				return constantExpression;
			}
			FreeNode(constantExpression);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypenameSpecifier(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TYPENAME))
			{
				if (Match(data, TokenType::COLON))
				{
					Consume(data, TokenType::COLON);
				}

				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(data, TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier(data);
				}
				if (nestedNameSpecifier->success)
				{
					if (Peek(data) == TokenType::IDENTIFIER)
					{
						return GenerateTypenameSpecifierNode(nestedNameSpecifier, ConsumeCurrent(data, TokenType::IDENTIFIER));
					}

					bool hasTemplateKeyword = Match(data, TokenType::KW_TEMPLATE);
					AstNode* simpleTemplateId = ParseSimpleTemplateId(data);
					if (simpleTemplateId->success)
					{
						return GenerateTypenameTemplateSpecifierNode(nestedNameSpecifier, simpleTemplateId, hasTemplateKeyword);
					}
					FreeNode(simpleTemplateId);
				}
				FreeNode(nestedNameSpecifier);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExplicitInstantiation(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			bool hasExternKeyword = Match(data, TokenType::KW_EXTERN);
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				AstNode* declaration = ParseDeclaration(data);
				if (declaration->success)
				{
					return GenerateExplicitInstantiationNode(declaration, hasExternKeyword);
				}
				FreeNode(declaration);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExplicitSpecialization(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TEMPLATE))
			{
				Consume(data, TokenType::LEFT_ANGLE_BRACKET);
				Consume(data, TokenType::RIGHT_ANGLE_BRACKET);
				AstNode* declaration = ParseDeclaration(data);
				if (declaration->success)
				{
					declaration;
				}
				FreeNode(declaration);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Exceptions
		static AstNode* ParseTryBlock(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TRY))
			{
				AstNode* compoundStatement = ParseCompoundStatement(data);
				if (compoundStatement->success)
				{
					AstNode* handlerSeq = ParseHandlerSequence(data);
					if (handlerSeq->success)
					{
						return GenerateTryBlockNode(compoundStatement, handlerSeq);
					}
					FreeNode(handlerSeq);
				}
				FreeNode(compoundStatement);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionTryBlock(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_TRY))
			{
				// Optional
				AstNode* ctorInitializer = ParseCtorInitializer(data);
				AstNode* compoundStatement = ParseCompoundStatement(data);
				if (compoundStatement->success)
				{
					AstNode* handlerSeq = ParseHandlerSequence(data);
					if (handlerSeq->success)
					{
						return GenerateFunctionTryBlockNode(ctorInitializer, compoundStatement, handlerSeq);
					}
					FreeNode(handlerSeq);
				}
				FreeNode(compoundStatement);
				FreeNode(ctorInitializer);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseHandlerSequence(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseHandler(data);
			if (!result->success)
			{
				BacktrackTo(data, backtrackPosition);
				FreeNode(result);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextHandler = ParseHandlerSequence(data);
				result = GenerateHandlerSeqNode(result, nextHandler);
				if (!nextHandler->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseHandler(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_CATCH))
			{
				Consume(data, TokenType::LEFT_PAREN);
				AstNode* exceptionDeclaration = ParseExceptionDeclaration(data);
				if (exceptionDeclaration->success)
				{
					Consume(data, TokenType::RIGHT_PAREN);
					AstNode* compoundStatement = ParseCompoundStatement(data);
					if (compoundStatement->success)
					{
						return GenerateHandlerNode(exceptionDeclaration, compoundStatement);
					}
					FreeNode(compoundStatement);
				}
				FreeNode(exceptionDeclaration);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExceptionDeclaration(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence(data);
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence(data);
			if (typeSpecifierSeq->success)
			{
				int backtrackPosition2 = data.CurrentToken;
				AstNode* declarator = ParseDeclarator(data);
				if (declarator->success)
				{
					return GenerateExceptionDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
				}
				FreeNode(declarator);
				BacktrackTo(data, backtrackPosition2);

				// Optional
				AstNode* abstractDeclarator = ParseAbstractDeclarator(data);
				return GenerateExceptionAbstractDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(typeSpecifierSeq);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseThrowExpression(ParserData& data)
		{
			if (Match(data, TokenType::KW_THROW))
			{
				// Optional
				AstNode* assignmentExpression = ParseAssignmentExpression(data);
				return GenerateThrowExpressionNode(assignmentExpression);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExceptionSpecification(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* noexceptSpecification = ParseNoexceptSpecification(data);
			if (noexceptSpecification->success)
			{
				return noexceptSpecification;
			}
			FreeNode(noexceptSpecification);
			BacktrackTo(data, backtrackPosition);

			AstNode* dynamicExceptionSpecification = ParseDynamicExceptionSpecification(data);
			if (dynamicExceptionSpecification->success)
			{
				return dynamicExceptionSpecification;
			}
			FreeNode(dynamicExceptionSpecification);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDynamicExceptionSpecification(ParserData& data)
		{
			if (Match(data, TokenType::KW_THROW))
			{
				Consume(data, TokenType::LEFT_PAREN);
				// Optional
				AstNode* typeIdList = ParseTypeIdList(data);
				Consume(data, TokenType::RIGHT_PAREN);

				return GenerateDynamicExceptionSpecNode(typeIdList);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeIdList(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			AstNode* result = ParseTypeId(data);
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(data, TokenType::COMMA))
			{
				result = GenerateTypeIdListNode(result, ParseTypeIdList(data));
			}

			if (Match(data, TokenType::DOT))
			{
				Consume(data, TokenType::DOT);
				Consume(data, TokenType::DOT);
			}

			return GenerateTypeIdListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseNoexceptSpecification(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::KW_NOEXCEPT))
			{
				if (Match(data, TokenType::LEFT_PAREN))
				{
					AstNode* constantExpression = ParseConstantExpression(data);
					if (constantExpression->success)
					{
						Consume(data, TokenType::RIGHT_PAREN);
						return GenerateNoexceptExpressionSpecNode(constantExpression);
					}
					FreeNode(constantExpression);
					return GenerateNoSuccessAstNode();
				}

				return GenerateNoexceptSpecNode();
			}

			return GenerateNoSuccessAstNode();
		}

		// ===============================================================================================
		// Parser Preprocessing Implementation (internal)
		//
		// These implement the grammar rules found at: https://www.nongnu.org/hcb/#decl-specifier-seq
		// It has been modified where needed.
		//
		// The including of files is kind of hacked together. I search through basic include directories
		// in Windows 10 right now and see if I can find the file, if it's not in the base file's directory
		// or any of the include directories specified. I just merge the tokens found in this file
		// into the tokens being processed right now.
		// ===============================================================================================
		// Preprocessing Stuff
		static void ExpandIncludes(const char* fileBeingParsed, const std::vector<std::filesystem::path>& includeDirs, ParserData& data);
		static List<Token> GetTokensInAbsoluteFile(const std::filesystem::path& filepath, const std::vector<std::filesystem::path>& includeDirs, ParserData& data)
		{
			std::string filepathStr = filepath.string();
			Logger::Info("Getting tokens from included file in '%s'", filepathStr.c_str());

			List<Token> tokensInFile = ScriptScanner::ScanTokens(filepathStr.c_str());
			tokensInFile.insert(CppTokens::CreateToken(-1, -1, TokenType::PREPROCESSING_FILE_BEGIN, filepathStr.c_str()), 0);
			tokensInFile.push(CppTokens::CreateToken(-1, -1, TokenType::PREPROCESSING_FILE_END, filepathStr.c_str()));
			ParserData preprocessedData = {
				tokensInFile,
				0
			};
			ExpandIncludes(filepathStr.c_str(), includeDirs, preprocessedData);

			return preprocessedData.Tokens;
		}

		static std::filesystem::path SearchForFile(const std::filesystem::path& baseDir, const char* filename)
		{
			std::filesystem::path potential = baseDir / std::filesystem::path(filename);
			if (std::filesystem::is_regular_file(potential) && potential.has_extension())
			{
				return potential;
			}
			else if (!potential.has_extension() && std::filesystem::is_regular_file(potential.replace_extension(".h")))
			{
				return potential.replace_extension(".h");
			}
			else if (potential.has_extension() && std::filesystem::is_regular_file(potential.replace_extension(".hpp")))
			{
				return potential.replace_extension(".hpp");
			}
			else if (potential.has_extension() && std::filesystem::is_regular_file(potential.replace_extension("")))
			{
				return potential.replace_extension("");
			}
			else
			{
				if (std::filesystem::is_directory(baseDir))
				{
					for (const std::filesystem::path& path : std::filesystem::directory_iterator(baseDir))
					{
						if (std::filesystem::is_directory(path))
						{
							std::filesystem::path recursiveSearch = SearchForFile(path, filename);
							if (recursiveSearch != "")
							{
								return recursiveSearch;
							}
						}
					}
				}
			}

			return "";
		}

		static std::filesystem::path SearchForFile(const Token& fileToken, const char* fileBeingParsed, const std::vector<std::filesystem::path>& includeDirs)
		{
			// Search the directory of the file being parsed
			std::filesystem::path currentFilePath = std::filesystem::absolute(fileBeingParsed);
			std::filesystem::path currentFileDir = currentFilePath.parent_path();
			std::filesystem::path potentialFilePath = currentFileDir / fileToken.m_Lexeme;
			if (std::filesystem::is_regular_file(potentialFilePath))
			{
				return potentialFilePath;
			}

			// Search include directories
			for (const std::filesystem::path& includeDir : includeDirs)
			{
				if (std::filesystem::is_regular_file(includeDir / fileToken.m_Lexeme))
				{
					return std::filesystem::absolute(includeDir / fileToken.m_Lexeme);
				}
			}

			// Search system default directories
			// TODO: Add support for Linux, Mac system default directories
			// TODO: Ideally we would build an index of the 'Program Files (x86)' and just use that to search for
			// TODO: the file...
			static const std::vector<std::filesystem::path> defaultDirs = {
				std::filesystem::path("C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC"),
				std::filesystem::path("C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Auxiliary/VS/include"),
				std::filesystem::path("C:/Program Files (x86)/Windows Kits/10/Include"),
				std::filesystem::path("C:/Program Files (x86)/Windows Kits/NETFXSDK")
			};

			for (const std::filesystem::path& path : defaultDirs)
			{
				std::filesystem::path searchedFile = SearchForFile(path, fileToken.m_Lexeme);
				if (searchedFile != "")
				{
					return searchedFile;
				}
			}

			return "";
		}

		static bool SeenFile(const std::filesystem::path& fileInQuestion, const std::vector<std::filesystem::path>& filesSeen)
		{
			for (const std::filesystem::path& file : filesSeen)
			{
				if (std::filesystem::equivalent(fileInQuestion, file))
				{
					return true;
				}
			}

			return false;
		}

		static void MergeTokensAtCurrentPosition(ParserData& data, const List<Token>& tokens)
		{
			if (tokens.size() > 0)
			{
				//auto currentPosition = data.Tokens.begin() + data.CurrentToken;
				data.Tokens.insert(tokens.begin(), tokens.end(), data.CurrentToken);
			}
		}

		static void PasteReplacementListHere(ParserData& data, const List<Token>& replacement, bool isFunctionMacro)
		{
			if (replacement.size() > 0)
			{
				auto currentPosition = data.Tokens.begin() + data.CurrentToken;
				data.Tokens.insert(replacement.begin(), replacement.end(), data.CurrentToken);
				//data.Tokens.insert(currentPosition, replacement.begin(), replacement.end());
				int macroTokenLength = 1;
				if (isFunctionMacro)
				{
					int tmpCursor = data.CurrentToken + replacement.size();
					int grouping = 0;
					Logger::Assert(tmpCursor + 1 < data.Tokens.size() && data.Tokens[tmpCursor + 1].m_Type == TokenType::LEFT_PAREN, "Invalid function macro. Must begin with a '('");
					while (tmpCursor < data.Tokens.size())
					{
						ParserString::FreeString(data.Tokens[tmpCursor].m_Lexeme);
						if (data.Tokens[tmpCursor].m_Type == TokenType::LEFT_PAREN)
						{
							grouping++;
						}
						else if (data.Tokens[tmpCursor].m_Type == TokenType::RIGHT_PAREN)
						{
							grouping--;
							if (grouping <= 0)
							{
								break;
							}
						}
						macroTokenLength++;
						tmpCursor++;
					}
				}
				//data.Tokens.erase(data.Tokens.begin() + data.CurrentToken + replacement.size(), data.Tokens.begin() + data.CurrentToken + replacement.size() + macroTokenLength);
				data.Tokens.removeRange(data.Tokens.begin() + data.CurrentToken + replacement.size(), data.Tokens.begin() + data.CurrentToken + replacement.size() + macroTokenLength);
			}
		}

		static int RemoveTokensAtLine(ParserData& data, int line, std::filesystem::path filepath)
		{
			int numTokensRemoved = 0;
			bool lookingInFile = false;
			for (auto tokenIter = data.Tokens.begin(); tokenIter != data.Tokens.end();)
			{
				if (tokenIter->m_Type == TokenType::PREPROCESSING_FILE_BEGIN && std::filesystem::path(tokenIter->m_Lexeme) == filepath)
				{
					lookingInFile = true;
				}
				if (tokenIter->m_Type == TokenType::PREPROCESSING_FILE_END && std::filesystem::path(tokenIter->m_Lexeme) == filepath)
				{
					// We have hit the end of this file, time to return
					return numTokensRemoved;
				}

				if (lookingInFile && tokenIter->m_Line == line)
				{
					ParserString::FreeString(tokenIter->m_Lexeme);
					tokenIter = data.Tokens.removeIter(tokenIter);
					numTokensRemoved++;
				}
				else
				{
					tokenIter++;
				}
			}
			return numTokensRemoved;
		}

		static void RemoveWhitespaceTokens(ParserData& data)
		{
			for (auto tokenIter = data.Tokens.begin(); tokenIter != data.Tokens.end();)
			{
				if (tokenIter->m_Type == TokenType::WHITESPACE || tokenIter->m_Type == TokenType::NEWLINE || tokenIter->m_Type == TokenType::COMMENT)
				{
					ParserString::FreeString(tokenIter->m_Lexeme);
					tokenIter = data.Tokens.removeIter(tokenIter);
				}
				else
				{
					tokenIter++;
				}
			}
		}

		static void RemoveSpecialTokens(ParserData& data)
		{
			for (auto tokenIter = data.Tokens.begin(); tokenIter != data.Tokens.end();)
			{
				if (tokenIter->m_Type == TokenType::PREPROCESSING_FILE_BEGIN || tokenIter->m_Type == TokenType::PREPROCESSING_FILE_END)
				{
					ParserString::FreeString(tokenIter->m_Lexeme);
					tokenIter = data.Tokens.removeIter(tokenIter);
				}
				else if (tokenIter->m_Type == TokenType::END_OF_FILE && tokenIter != data.Tokens.end() - 1)
				{
					ParserString::FreeString(tokenIter->m_Lexeme);
					tokenIter = data.Tokens.removeIter(tokenIter);
				}
				else
				{
					tokenIter++;
				}
			}
		}

		static void walkSimpleMacroDefine(PreprocessingAstNode* node)
		{
			Symbols::AddDefineSymbol(PreprocessingSymbolTable, node->macroDefine.identifier, node->macroDefine.identifier.m_Line, node);
		}

		static void walkMacroDefineFunction(PreprocessingAstNode* node)
		{
			Symbols::AddDefineSymbol(PreprocessingSymbolTable, node->macroDefine.identifier, node->macroDefine.identifier.m_Line, node);
		}

		static void walkMacroUndefine(PreprocessingAstNode* node)
		{
			Symbols::AddUndefine(PreprocessingSymbolTable, node->macroUndef.identifier, node->macroUndef.identifier.m_Line);
		}

		static void ExpandIncludes(const char* fileBeingParsed, const std::vector<std::filesystem::path>& includeDirs, ParserData& data)
		{
			int tokensSize = data.Tokens.size();

			// Get all the #includes included
			while (data.CurrentToken < tokensSize)
			{
				if (GetCurrentToken(data).m_Type == TokenType::HASHTAG)
				{
					PreprocessingAstNode* includeFile = ParseMacroInclude(data);
					if (includeFile->success)
					{
						PreprocessingAstNode* ppToken = includeFile->macroInclude.ppTokens->ppTokens.preprocessingToken;
						Token file;
						if (ppToken->type == PreprocessingAstNodeType::HeaderName)
						{
							file = ppToken->headerName.identifier;
						}
						else
						{
							file = ppToken->headerNameString.stringLiteral;
						}

						std::filesystem::path absFile = SearchForFile(file, fileBeingParsed, includeDirs);
						if (absFile != "" && !SeenFile(absFile, FilesSeen))
						{
							FilesSeen.emplace_back(absFile);
							MergeTokensAtCurrentPosition(data, GetTokensInAbsoluteFile(absFile, includeDirs, data));
							// The size of the tokens has just grown
							tokensSize = data.Tokens.size();
						}
						if (absFile == "")
						{
							Logger::Warning("Unable to find file '%s'", file.m_Lexeme);
						}
						data.CurrentToken -= RemoveTokensAtLine(data, file.m_Line, fileBeingParsed);
						data.CurrentToken = std::max(data.CurrentToken, 0);
					}
					else
					{
						data.CurrentToken++;
					}
				}
				else
				{
					data.CurrentToken++;
				}
			}
		}

		static void ExpandDefineMacros(ParserData& data)
		{
			int tokensSize = data.Tokens.size();
			data.CurrentToken = 0;
			PreprocessingAstNode* preprocessedTree = ParsePreprocessingFile(data);
			WalkPreprocessingTree(preprocessedTree, walkSimpleMacroDefine, PreprocessingAstNodeType::MacroDefine, false);
			WalkPreprocessingTree(preprocessedTree, walkMacroDefineFunction, PreprocessingAstNodeType::MacroDefineFunction, false);
			WalkPreprocessingTree(preprocessedTree, walkMacroUndefine, PreprocessingAstNodeType::MacroUndef, false);

			data.CurrentToken = 0;
			while (data.CurrentToken < tokensSize)
			{
				Token& token = GetCurrentToken(data);
				if (GetCurrentToken(data).m_Type == TokenType::HASHTAG)
				{
					data.CurrentToken++;
					// Skip all #define lines and #undef lines, that way we don't expand the macro early
					if (GetCurrentToken(data).m_Type == TokenType::IDENTIFIER &&
						(ParserString::Compare(GetCurrentToken(data).m_Lexeme, "undef") || ParserString::Compare(GetCurrentToken(data).m_Lexeme, "define")))
					{
						int currentLine = token.m_Line;
						while (!AtEnd(data) && GetCurrentToken(data).m_Line == currentLine)
						{
							data.CurrentToken++;
						}
					}
				}
				else if (GetCurrentToken(data).m_Type == TokenType::IDENTIFIER && Symbols::IsSymbol(PreprocessingSymbolTable, token))
				{
					List<Token> replacement = Symbols::ExpandMacro(PreprocessingSymbolTable, data.CurrentToken, data.Tokens);
					if (replacement.size() > 0)
					{
						PasteReplacementListHere(data, replacement, Symbols::IsFunctionMacroDefine(PreprocessingSymbolTable, token));
						tokensSize = data.Tokens.size();
					}
					else
					{
						data.CurrentToken++;
					}
				}
				else
				{
					data.CurrentToken++;
				}
			}
		}

		static void Preprocess(const char* fileBeingParsed, const std::vector<std::filesystem::path>& includeDirs, ParserData& data)
		{
			// Get all the #includes included
			data.CurrentToken = 0;
			ExpandIncludes(fileBeingParsed, includeDirs, data);
			RemoveSpecialTokens(data);

			// Expand all macros
			ExpandDefineMacros(data);

			RemoveWhitespaceTokens(data);
		}

		static PreprocessingAstNode* ParsePreprocessingFile(ParserData& data)
		{
			return GeneratePreprocessingFileNode(ParseGroup(data));
		}

		static PreprocessingAstNode* ParseGroup(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* result = ParseGroupPart(data);
			if (!result->success)
			{
				FreePreprocessingNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessPreprocessingAstNode();
			}

			while (!AtEnd(data))
			{
				PreprocessingAstNode* nextGroup = ParseGroup(data);
				result = GenerateGroupNode(result, nextGroup);
				if (!nextGroup->success)
				{
					break;
				}
			}

			if (AtEnd(data))
			{
				result = GenerateGroupNode(result, GenerateNoSuccessPreprocessingAstNode());
			}

			return result;
		}

		static PreprocessingAstNode* ParseGroupPart(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* ifSection = ParseIfSection(data);
			if (ifSection->success)
			{
				return ifSection;
			}
			FreePreprocessingNode(ifSection);
			BacktrackTo(data, backtrackPosition);

			PreprocessingAstNode* controlLine = ParseControlLine(data);
			if (controlLine->success)
			{
				return controlLine;
			}
			FreePreprocessingNode(controlLine);
			BacktrackTo(data, backtrackPosition);

			PreprocessingAstNode* textLine = ParseTextLine(data);
			if (textLine->success)
			{
				return textLine;
			}
			FreePreprocessingNode(textLine);
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseIfSection(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* ifGroup = ParseIfGroup(data);
			if (ifGroup->success)
			{
				// Optional
				PreprocessingAstNode* elifGroups = ParseElifGroups(data);
				PreprocessingAstNode* elseGroup = ParseElseGroup(data);
				Consume(data, TokenType::HASHTAG);
				Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
				Logger::Assert(ParserString::Compare(identifier.m_Lexeme, "endif"), "Expected '#endif'");
				return GenerateIfSectionNode(ifGroup, elifGroups, elseGroup);
			}

			FreePreprocessingNode(ifGroup);
			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseIfGroup(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::HASHTAG))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);

					int backtrackPosition2 = data.CurrentToken;
					if (ParserString::Compare(identifier.m_Lexeme, "if"))
					{
						AstNode* constantExpression = ParseConstantExpression(data);
						if (constantExpression->success)
						{
							// optional
							Consume(data, TokenType::NEWLINE);
							PreprocessingAstNode* group = ParseGroup(data);
							return GenerateIfGroupNode(constantExpression, group);
						}
						FreeNode(constantExpression);
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "ifdef"))
					{
						if (Peek(data) == TokenType::IDENTIFIER)
						{
							Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
							Consume(data, TokenType::NEWLINE);
							// Optional
							PreprocessingAstNode* group = ParseGroup(data);
							return GenerateIfDefGroupNode(identifier, group);
						}
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "ifndef"))
					{
						if (Peek(data) == TokenType::IDENTIFIER)
						{
							Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
							Consume(data, TokenType::NEWLINE);
							// Optional
							PreprocessingAstNode* group = ParseGroup(data);
							return GenerateIfNDefGroupNode(identifier, group);
						}
					}

				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseElifGroups(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* result = ParseElifGroup(data);
			if (!result->success)
			{
				FreePreprocessingNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessPreprocessingAstNode();
			}

			while (true)
			{
				PreprocessingAstNode* nextGroup = ParseElifGroups(data);
				result = GenerateElifGroupsNode(result, nextGroup);
				if (!nextGroup->success)
				{
					break;
				}
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseElifGroup(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::HASHTAG))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);

					if (ParserString::Compare(identifier.m_Lexeme, "elif"))
					{
						AstNode* constantExpression = ParseConstantExpression(data);
						if (constantExpression->success)
						{
							Consume(data, TokenType::NEWLINE);
							// Optional
							PreprocessingAstNode* group = ParseGroup(data);
							return GenerateElifGroupNode(constantExpression, group);
						}
						FreeNode(constantExpression);
					}
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseElseGroup(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::HASHTAG))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);

					if (ParserString::Compare(identifier.m_Lexeme, "else"))
					{
						Consume(data, TokenType::NEWLINE);
						// Optional
						PreprocessingAstNode* group = ParseGroup(data);
						return GenerateElseGroupNode(group);
					}
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseMacroInclude(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			if (Match(data, TokenType::HASHTAG))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					if (ParserString::Compare(identifier.m_Lexeme, "include"))
					{
						PreprocessingAstNode* ppTokens = ParsePPTokens(data, true);
						if (ppTokens->success)
						{
							Consume(data, TokenType::NEWLINE);
							return GenerateMacroIncludeNode(ppTokens);
						}
					}
				}
			}
			BacktrackTo(data, backtrackPosition);

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseControlLine(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* macroInclude = ParseMacroInclude(data);
			if (macroInclude->success)
			{
				return macroInclude;
			}
			BacktrackTo(data, backtrackPosition);
			FreePreprocessingNode(macroInclude);

			if (Match(data, TokenType::HASHTAG))
			{
				if (Peek(data) == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
					int backtrackPosition2 = data.CurrentToken;

					if (ParserString::Compare(identifier.m_Lexeme, "define"))
					{
						if (Peek(data) == TokenType::IDENTIFIER)
						{
							Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
							// TODO: make sure this left parenthisis is not preceded by whitespace
							if (Match(data, TokenType::LEFT_PAREN))
							{
								PreprocessingAstNode* identifierList = ParseIdentifierList(data);
								if (identifierList->success)
								{
									Match(data, TokenType::COMMA);
									if (Match(data, TokenType::DOT))
									{
										Consume(data, TokenType::DOT);
										Consume(data, TokenType::DOT);
									}
								}

								Consume(data, TokenType::RIGHT_PAREN);
								// TODO: Something is going wrong here, because of parentheses??
								PreprocessingAstNode* replacementList = ParseReplacementList(data);
								if (replacementList->success)
								{
									Consume(data, TokenType::NEWLINE);
									return GenerateMacroDefineFunctionNode(identifier, identifierList, replacementList);
								}
								FreePreprocessingNode(replacementList);
								FreePreprocessingNode(identifierList);
							}

							PreprocessingAstNode* replacementList = ParseReplacementList(data);
							if (replacementList->success)
							{
								Consume(data, TokenType::NEWLINE);
								return GenerateMacroDefineNode(identifier, replacementList);
							}
							FreePreprocessingNode(replacementList);
						}
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "undef"))
					{
						if (Peek(data) == TokenType::IDENTIFIER)
						{
							Token identifier = ConsumeCurrent(data, TokenType::IDENTIFIER);
							Consume(data, TokenType::NEWLINE);
							return GenerateMacroUndefNode(identifier);
						}
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "line"))
					{
						PreprocessingAstNode* ppTokens = ParsePPTokens(data);
						if (ppTokens->success)
						{
							Consume(data, TokenType::NEWLINE);
							return GenerateMacroLineNode(ppTokens);
						}
						FreePreprocessingNode(ppTokens);
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "error"))
					{
						// Optional
						PreprocessingAstNode* ppTokens = ParsePPTokens(data);
						Consume(data, TokenType::NEWLINE);
						return GenerateMacroErrorNode(ppTokens);
					}
					BacktrackTo(data, backtrackPosition2);

					if (ParserString::Compare(identifier.m_Lexeme, "pragma"))
					{
						// Optional
						PreprocessingAstNode* ppTokens = ParsePPTokens(data);
						Consume(data, TokenType::NEWLINE);
						return GenerateMacroPragmaNode(ppTokens);
					}
					BacktrackTo(data, backtrackPosition2);

					if (Match(data, TokenType::NEWLINE))
					{
						return GenerateEmptyMacroNode();
					}
				}
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseTextLine(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			// Optional
			PreprocessingAstNode* ppTokens = ParsePPTokens(data);
			if (Match(data, TokenType::NEWLINE))
			{
				return GenerateTextLineNode(ppTokens);
			}

			BacktrackTo(data, backtrackPosition);
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseNonDirective(ParserData& data)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* ppTokens = ParsePPTokens(data);
			if (ppTokens->success)
			{
				Consume(data, TokenType::NEWLINE);
				return GenerateNonDirectiveNode(ppTokens);
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseIdentifierList(ParserData& data)
		{
			if (Peek(data) == TokenType::IDENTIFIER)
			{
				PreprocessingAstNode* result = GenerateIdentifierNode(ConsumeCurrent(data, TokenType::IDENTIFIER));

				while (Match(data, TokenType::COMMA))
				{
					result = GenerateIdentifierListNode(result, ParseIdentifierList(data));
				}

				return result;
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseReplacementList(ParserData& data)
		{
			// Optional
			PreprocessingAstNode* ppTokens = ParsePPTokens(data);
			return GenerateReplacementListNode(ppTokens);
		}

		static PreprocessingAstNode* ParsePPTokens(ParserData& data, bool isHeader)
		{
			int backtrackPosition = data.CurrentToken;
			PreprocessingAstNode* result = ParsePreprocessingToken(data, isHeader);
			if (!result->success)
			{
				FreePreprocessingNode(result);
				BacktrackTo(data, backtrackPosition);
				return GenerateNoSuccessPreprocessingAstNode();
			}

			while (!AtEnd(data))
			{
				PreprocessingAstNode* nextPPToken = ParsePPTokens(data, isHeader);
				result = GeneratePPTokensNode(result, nextPPToken);
				if (!nextPPToken->success)
				{
					break;
				}
			}

			if (AtEnd(data))
			{
				result = GeneratePPTokensNode(result, GenerateNoSuccessPreprocessingAstNode());
			}

			return result;
		}

		static PreprocessingAstNode* ParseNumberLiteral(ParserData& data)
		{
			if (Peek(data) == TokenType::FLOATING_POINT_LITERAL || Peek(data) == TokenType::INTEGER_LITERAL)
			{
				return GenerateNumberLiteralNode(ConsumeCurrent(data, Peek(data)));
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		// Preprocessor Stuff
		static PreprocessingAstNode* ParsePreprocessingToken(ParserData& data, bool isHeader)
		{
			if (Peek(data) == TokenType::NEWLINE || Peek(data) == TokenType::END_OF_FILE)
			{
				return GenerateNoSuccessPreprocessingAstNode();
			}

			int backtrackPosition = data.CurrentToken;
			if (isHeader)
			{
				PreprocessingAstNode* headerName = ParseHeaderName(data);
				if (headerName->success)
				{
					return headerName;
				}
				FreePreprocessingNode(headerName);
				BacktrackTo(data, backtrackPosition);
			}

			if (Peek(data) == TokenType::IDENTIFIER)
			{
				return GenerateIdentifierNode(ConsumeCurrent(data, TokenType::IDENTIFIER));
			}

			PreprocessingAstNode* numberLiteral = ParseNumberLiteral(data);
			if (numberLiteral->success)
			{
				return numberLiteral;
			}
			FreePreprocessingNode(numberLiteral);
			BacktrackTo(data, backtrackPosition);

			PreprocessingAstNode* characterLiteral = ParseCharacterLiteral(data);
			if (characterLiteral->success)
			{
				return characterLiteral;
			}
			FreePreprocessingNode(characterLiteral);
			BacktrackTo(data, backtrackPosition);

			PreprocessingAstNode* stringLiteral = ParseStringLiteral(data);
			if (stringLiteral->success)
			{
				return stringLiteral;
			}
			FreePreprocessingNode(stringLiteral);
			BacktrackTo(data, backtrackPosition);

			PreprocessingAstNode* preprocessingOpOrPunc = ParsePreprocessingOpOrPunc(data);
			if (preprocessingOpOrPunc->success)
			{
				return preprocessingOpOrPunc;
			}
			FreePreprocessingNode(preprocessingOpOrPunc);
			BacktrackTo(data, backtrackPosition);

			// If nothing else succeeds, just treat the current token as an identifier because
			// it's not a newline and it's not an EOF token
			return GenerateIdentifierNode(ConsumeCurrent(data, Peek(data)));
		}

		static PreprocessingAstNode* ParseHeaderName(ParserData& data)
		{
			if (Match(data, TokenType::LEFT_ANGLE_BRACKET))
			{
				Token identifier = ConsumeCurrent(data, Peek(data));
				identifier.m_Lexeme = ParserString::CreateString(identifier.m_Lexeme);
				while (!AtEnd(data) && !Match(data, TokenType::RIGHT_ANGLE_BRACKET))
				{
					if (Match(data, TokenType::NEWLINE))
					{
						Logger::Error("Invalid #include definition. Must have a '>' before newline.");
					}

					// Join tokens and erase invalid tokens, it's a mess
					int tokenPos = data.CurrentToken;
					Token token = ConsumeCurrent(data, Peek(data));
					const char* oldStr = identifier.m_Lexeme;
					identifier.m_Lexeme = ParserString::Join(identifier.m_Lexeme, token.m_Lexeme);
					ParserString::FreeString(oldStr);
				}
				return GenerateHeaderNameNode(identifier);
			}

			if (Peek(data) == TokenType::STRING_LITERAL)
			{
				Token stringLiteral = ConsumeCurrent(data, TokenType::STRING_LITERAL);
				return GenerateHeaderNameStringNode(stringLiteral);
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseCharacterLiteral(ParserData& data)
		{
			if (Peek(data) == TokenType::CHARACTER_LITERAL)
			{
				return GenerateCharacterLiteralNode(ConsumeCurrent(data, TokenType::CHARACTER_LITERAL));
			}
			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseStringLiteral(ParserData& data)
		{
			if (Peek(data) == TokenType::STRING_LITERAL)
			{
				Token stringLiteral = ConsumeCurrent(data, TokenType::STRING_LITERAL);
				return GenerateStringLiteralNode(stringLiteral);
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParsePreprocessingOpOrPunc(ParserData& data)
		{
			if (PeekIn(data, { TokenType::LEFT_CURLY_BRACKET, TokenType::RIGHT_CURLY_BRACKET, TokenType::LEFT_BRACKET, TokenType::RIGHT_BRACKET, TokenType::HASHTAG,
				TokenType::LEFT_PAREN, TokenType::RIGHT_PAREN, TokenType::LEFT_ANGLE_BRACKET, TokenType::RIGHT_ANGLE_BRACKET, TokenType::COLON, TokenType::MODULO,
				TokenType::SEMICOLON, TokenType::DOT, TokenType::KW_NEW, TokenType::KW_DELETE, TokenType::QUESTION, TokenType::STAR, TokenType::STAR, TokenType::ARROW,
				TokenType::TILDE, TokenType::BANG, TokenType::PLUS, TokenType::DIV, TokenType::CARET, TokenType::AND, TokenType::PIPE, TokenType::EQUAL,
				TokenType::PLUS_EQUAL, TokenType::MINUS_EQUAL, TokenType::STAR_EQUAL, TokenType::DIV_EQUAL, TokenType::MODULO_EQUAL, TokenType::CARET_EQUAL, TokenType::AND_EQUAL,
				TokenType::PIPE_EQUAL, TokenType::LEFT_SHIFT, TokenType::RIGHT_SHIFT, TokenType::EQUAL_EQUAL, TokenType::BANG_EQUAL, TokenType::LESS_THAN_EQ, TokenType::GREATER_THAN_EQ,
				TokenType::LOGICAL_AND, TokenType::LOGICAL_OR, TokenType::LEFT_SHIFT_EQUAL, TokenType::RIGHT_SHIFT_EQUAL, TokenType::PLUS_PLUS, TokenType::MINUS_MINUS,
				TokenType::COMMA }))
			{
				return GeneratePreprocessingOpOrPuncNode(ConsumeCurrent(data, Peek(data)));
			}

			return GenerateNoSuccessPreprocessingAstNode();
		}

		static PreprocessingAstNode* ParseHCharSequence(ParserData& data);
		static PreprocessingAstNode* ParseHChar(ParserData& data);
		static PreprocessingAstNode* ParseQCharSequence(ParserData& data);
		static PreprocessingAstNode* ParseQChar(ParserData& data);
	}
}
