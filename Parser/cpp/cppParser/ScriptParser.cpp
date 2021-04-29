#include "cppParser/ScriptParser.h"
#include "cppParser/CppTokens.h"
#include "cppParser/Ast.h"
#include "cppParser/ScriptScanner.h"

namespace CppParser
{
	namespace Parser
	{
		// Internal variables
		static CPP_PARSER_VECTOR(Token) Tokens;
		static int CurrentToken = 0;

		static AstNode* ParseTranslationUnit();

		AstNode* Parse(CPP_PARSER_VECTOR(Token)& tokens)
		{
			CurrentToken = 0;
			Tokens = tokens;

			AstNode* result = ParseTranslationUnit();
			return result;
		}

		static void FreeNodeCallback(AstNode* tree)
		{
			FreeMem(tree);
		}

		static void FreeNode(AstNode* node)
		{
			WalkTree(node, FreeNodeCallback);
		}

		void FreeTree(AstNode* tree)
		{
			FreeNode(tree);
		}

		void WalkTree(AstNode* tree, void(*callbackFn)(AstNode* node), AstNodeType notificationType)
		{
#define WALK(node) WalkTree(node, callbackFn, notificationType)
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
				CPP_PARSER_LOG_ERROR("Unknown tree node type: '%d' while walking tree.", (int)tree->type);
				break;
			}

			if (notificationType == AstNodeType::All || tree->type == notificationType)
			{
				callbackFn(tree);
			}
#undef WALK
		}

		static bool AtEnd()
		{
			return CurrentToken == Tokens.size();
		}

		static void ErrorAtToken()
		{
			Token& currentToken = AtEnd() ? Tokens[Tokens.size() - 1] : Tokens[CurrentToken];
			CPP_PARSER_LOG_ERROR("Unexpected token '%s' at line %d:%d", ScriptScanner::TokenName(currentToken.m_Type), currentToken.m_Line, currentToken.m_Column);
		}

		static void Consume(TokenType type)
		{
			Token& currentToken = Tokens[CurrentToken];
			if (currentToken.m_Type == type)
			{
				CurrentToken++;
				return;
			}

			CPP_PARSER_LOG_ERROR("Unexpected token. Expected '%s' instead got '%s' at line: %d:%d", ScriptScanner::TokenName(type),
				ScriptScanner::TokenName(currentToken.m_Type), currentToken.m_Line, currentToken.m_Column);
		}

		static void BacktrackTo(int position)
		{
			CPP_PARSER_LOG_ASSERT(position >= 0 && position < Tokens.size(), "Invalid backtrack location.");
			CurrentToken = position;
		}

		static bool Match(TokenType type)
		{
			Token& currentToken = AtEnd() ? Tokens[Tokens.size() - 1] : Tokens[CurrentToken];
			if (currentToken.m_Type == type)
			{
				CurrentToken++;
				return true;
			}

			return false;
		}

		static Token ConsumeCurrent(TokenType type)
		{
			Token& currentToken = AtEnd() ? Tokens[Tokens.size() - 1] : Tokens[CurrentToken];
			if (currentToken.m_Type == type)
			{
				CurrentToken++;
				return currentToken;
			}

			CPP_PARSER_LOG_ERROR("Unexpected token. Expected '%s' instead got '%s'",
				ScriptScanner::TokenName(type),
				ScriptScanner::TokenName(currentToken.m_Type));
			return currentToken;
		}

		static Token GetCurrentToken()
		{
			return AtEnd() ? Tokens[Tokens.size() - 1] : Tokens[CurrentToken];
		}

		static bool IsAssignmentOperator(TokenType type)
		{
			return type == TokenType::EQUAL || type == TokenType::STAR_EQUAL || type == TokenType::DIV_EQUAL || type == TokenType::MODULO_EQUAL ||
				type == TokenType::PLUS_EQUAL || type == TokenType::MINUS_EQUAL || type == TokenType::RIGHT_SHIFT_EQUAL || type == TokenType::LEFT_SHIFT_EQUAL ||
				type == TokenType::AND_EQUAL || type == TokenType::CARET_EQUAL || type == TokenType::PIPE_EQUAL;
		}

		static AstNode* GenerateAstNode(AstNodeType type)
		{
			AstNode* node = (AstNode*)AllocMem(sizeof(AstNode));
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

		// TODO: Add this stuff in once I write a preprocessing engine
		/*static AstNode* GeneratePreprocessingFileNode(AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PreprocessingFile);
			result->preprocessingFile.group = group;
			return result;
		}

		static AstNode* GenerateGroupNode(AstNode* thisGroupPart, AstNode* nextGroupPart)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Group);
			result->group.thisGroupPart = thisGroupPart;
			result->group.nextGroupPart = nextGroupPart;
			return result;
		}

		static AstNode* GenerateIfSectionNode(AstNode* ifGroup, AstNode* elifGroups, AstNode* elseGroup)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IfSection);
			result->ifSection.ifGroup = ifGroup;
			result->ifSection.elifGroups = elifGroups;
			result->ifSection.elseGroup = elseGroup;
			return result;
		}

		static AstNode* GenerateIfGroupNode(AstNode* constantExpression, AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IfGroup);
			result->ifGroup.constantExpression = constantExpression;
			result->ifGroup.group = group;
			return result;
		}

		static AstNode* GenerateIfDefGroupNode(Token identifier, AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IfDefGroup);
			result->ifDefGroup.identifier = identifier;
			result->ifDefGroup.group = group;
			return result;
		}

		static AstNode* GenerateIfNDefGroupNode(Token identifier, AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IfNDefGroup);
			result->ifNDefGroup.identifier = identifier;
			result->ifNDefGroup.group = group;
			return result;
		}

		static AstNode* GenerateElifGroupsNode(AstNode* thisElifGroup, AstNode* nextElifGroup)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElifGroups);
			result->elifGroups.thisElifGroup = thisElifGroup;
			result->elifGroups.nextElifGroup = nextElifGroup;
			return result;
		}

		static AstNode* GenerateElifGroupNode(AstNode* constantExpression, AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElifGroup);
			result->elifGroup.constantExpression = constantExpression;
			result->elifGroup.group = group;
			return result;
		}

		static AstNode* GenerateElseGroupNode(AstNode* group)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ElseGroup);
			result->elseGroup.group = group;
			return result;
		}

		static AstNode* GenerateMacroIncludeNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroInclude);
			result->macroInclude.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateMacroDefineNode(Token identifier, AstNode* replacementList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroDefine);
			result->macroDefine.identifier = identifier;
			result->macroDefine.replacementList = replacementList;
			return result;
		}

		static AstNode* GenerateMacroDefineFunctionNode(Token identifier, AstNode* identifierList, AstNode* replacementList)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroDefineFunction);
			result->macroDefineFunction.identifier = identifier;
			result->macroDefineFunction.identifierList = identifierList;
			result->macroDefineFunction.replacementList = replacementList;
			return result;
		}

		static AstNode* GenerateMacroUndefNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroUndef);
			result->macroUndef.identifier = identifier;
			return result;
		}

		static AstNode* GenerateMacroLineNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroLine);
			result->macroLine.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateMacroErrorNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroError);
			result->macroError.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateMacroPragmaNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::MacroPragma);
			result->macroPragma.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateTextLineNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::TextLine);
			result->textLine.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateNonDirectiveNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NonDirective);
			result->nonDirective.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GenerateIdentifierListNode(AstNode* thisIdentifierNode, AstNode* nextIdentifierNode)
		{
			AstNode* result = GenerateAstNode(AstNodeType::IdentifierList);
			result->identifierList.thisIdentifierNode = thisIdentifierNode;
			result->identifierList.nextIdentifierNode = nextIdentifierNode;
			return result;
		}

		static AstNode* GenerateIdentifierNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::Identifier);
			result->identifier.identifier = identifier;
			return result;
		}

		static AstNode* GenerateReplacementListNode(AstNode* ppTokens)
		{
			AstNode* result = GenerateAstNode(AstNodeType::ReplacementList);
			result->replacementList.ppTokens = ppTokens;
			return result;
		}

		static AstNode* GeneratePPTokensNode(AstNode* preprocessingToken, AstNode* nextPreprocessingToken)
		{
			AstNode* result = GenerateAstNode(AstNodeType::PPTokens);
			result->ppTokens.preprocessingToken = preprocessingToken;
			result->ppTokens.nextPreprocessingToken = nextPreprocessingToken;
			return result;
		}

		static AstNode* GenerateStringLiteralNode(Token stringLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::StringLiteral);
			result->stringLiteral.stringLiteral = stringLiteral;
			return result;
		}

		static AstNode* GenerateNumberLiteralNode(Token numberLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::NumberLiteral);
			result->numberLiteral.numberLiteral = numberLiteral;
			return result;
		}

		static AstNode* GenerateCharacterLiteralNode(Token characterLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::CharacterLiteral);
			result->characterLiteral.characterLiteral = characterLiteral;
			return result;
		}

		static AstNode* GenerateHeaderNameNode(Token identifier)
		{
			AstNode* result = GenerateAstNode(AstNodeType::HeaderName);
			result->headerName.identifier = identifier;
			return result;
		}

		static AstNode* GenerateHeaderNameStringNode(Token stringLiteral)
		{
			AstNode* result = GenerateAstNode(AstNodeType::HeaderNameString);
			result->headerNameString.stringLiteral = stringLiteral;
			return result;
		}*/

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

		static TokenType Peek()
		{
			return AtEnd() ? Tokens[Tokens.size() - 1].m_Type : Tokens[CurrentToken].m_Type;
		}

		static bool PeekIn(std::initializer_list<TokenType> tokenTypes)
		{
			if (std::find(tokenTypes.begin(), tokenTypes.end(), Peek()) != tokenTypes.end())
			{
				return true;
			}
			return false;
		}

		static bool LookAheadBeforeSemicolon(std::initializer_list<TokenType> tokenTypes)
		{
			// This function looks for a token that matches any of the types in the initializer list
			// before the first semicolon or eof token. If it finds it, it returns true, otherwise false
			int token = CurrentToken;
			int tokenTypeSize = tokenTypes.size();
			while (!AtEnd())
			{
				Token& iter = Tokens[token];
				if (iter.m_Type == TokenType::SEMICOLON)
				{
					return false;
				}

				if (std::find(tokenTypes.begin(), tokenTypes.end(), iter.m_Type) != tokenTypes.end())
				{
					return true;
				}
				token++;
				if (token >= Tokens.size())
				{
					return false;
				}
			}
		}

		static bool MatchBeforeSemicolon(TokenType type1, TokenType nextType)
		{
			// This function looks for a token that matches any of the types in the initializer list
			// before the first semicolon or eof token. If it finds it, it returns true, otherwise false
			int token = CurrentToken;
			while (!AtEnd())
			{
				Token& iter = Tokens[token];
				if (iter.m_Type == TokenType::SEMICOLON)
				{
					return false;
				}

				if (iter.m_Type == type1 && token < Tokens.size() && Tokens[token + 1].m_Type == nextType)
				{
					return true;
				}
				token++;
				if (token >= Tokens.size())
				{
					return false;
				}
			}
		}

		// Translation Unit
		static AstNode* ParseTranslationUnit();

		// Expressions
		static AstNode* ParsePrimaryExpression();
		static AstNode* ParseIdExpression();
		static AstNode* ParseUnqualifiedId();
		static AstNode* ParseQualifiedId();
		static AstNode* ParseNestedNameSpecifier();

		// Lambdas
		static AstNode* ParseLambdaExpression();
		static AstNode* ParseLambdaIntroducer();
		static AstNode* ParseLambdaCapture();
		static AstNode* ParseCaptureList();
		static AstNode* ParseCapture();
		static AstNode* ParseLambdaDeclarator();

		// Postfix Expressions
		static AstNode* ParsePostfixExpression();
		static AstNode* ParseExpressionList();
		static AstNode* ParsePseudoDestructorName();

		// Unary Expressions
		static AstNode* ParseUnaryExpression();

		// New Expressions
		static AstNode* ParseNewExpression();
		static AstNode* ParseNewPlacement();
		static AstNode* ParseNewTypeId();
		static AstNode* ParseNewDeclarator();
		static AstNode* ParseNoptrNewDeclarator();
		static AstNode* ParseNewInitializer();

		// Delete 
		static AstNode* ParseDeleteExpression();

		// Noexcept
		static AstNode* ParseNoexceptExpression();

		// Cast
		static AstNode* ParseCastExpression();

		// Pointer to member
		static AstNode* ParsePmExpression();

		// Primary operations
		static AstNode* ParseMultiplicativeExpression();
		static AstNode* ParseAdditiveExpression();
		static AstNode* ParseShiftExpression();

		// Comparision operations
		static AstNode* ParseRelationalExpression();
		static AstNode* ParseEqualityExpression();
		static AstNode* ParseAndExpression();

		// Logical operations
		static AstNode* ParseExclusiveOrExpression();
		static AstNode* ParseInclusiveOrExpression();
		static AstNode* ParseLogicalAndExpression();
		static AstNode* ParseLogicalOrExpression();

		// Misc expressions
		static AstNode* ParseConditionalExpression();
		static AstNode* ParseAssignmentExpression();
		static AstNode* ParseAlignmentExpression();

		static AstNode* ParseExpression();
		static AstNode* ParseConstantExpression();

		// Statements
		static AstNode* ParseStatement();
		static AstNode* ParseLabeledStatement();
		static AstNode* ParseExpressionStatement();
		static AstNode* ParseCompoundStatement();
		static AstNode* ParseStatementSequence();

		// Selection statements
		static AstNode* ParseSelectionStatement();
		static AstNode* ParseCondition();

		// Iteration statements
		static AstNode* ParseIterationStatement();
		static AstNode* ParseForInitStatement();
		static AstNode* ParseForRangeDeclaration();
		static AstNode* ParseForRangeInitializer();

		// Jump statements
		static AstNode* ParseJumpStatement();

		// Declarations
		static AstNode* ParseDeclarationStatement();
		static AstNode* ParseDeclarationSequence();
		static AstNode* ParseUSystemDeclaration();
		static AstNode* ParseDeclaration();
		static AstNode* ParseBlockDeclaration();
		static AstNode* ParseAliasDeclaration();
		static AstNode* ParseSimpleDeclaration();
		static AstNode* ParseStaticAssertDeclaration();
		static AstNode* ParseEmptyDeclaration();
		static AstNode* ParseAttributeDeclaration();

		static AstNode* ParseDeclarationSpecifier();
		static AstNode* ParseDeclarationSpecifierSequence();
		static AstNode* ParseStorageClassSpecifier();
		static AstNode* ParseFunctionSpecifier();

		// Types/typedefs
		static AstNode* ParseTypedefName();
		static AstNode* ParseTypeSpecifier();
		static AstNode* ParseTrailingTypeSpecifier();
		static AstNode* ParseTypeSpecifierSequence();
		static AstNode* ParseTrailingTypeSpecifierSequence();

		static AstNode* ParseSimpleTypeSpecifier();
		static AstNode* ParseTypeName();
		static AstNode* ParseDecltypeSpecifier();
		static AstNode* ParseElaboratedTypeSpecifier();

		// Enums
		static AstNode* ParseEnumName();
		static AstNode* ParseEnumSpecifier();
		static AstNode* ParseEnumHead();
		static AstNode* ParseOpaqueEnumDeclaration();
		static AstNode* ParseEnumKey();
		static AstNode* ParseEnumBase();
		static AstNode* ParseEnumeratorList();
		static AstNode* ParseEnumeratorDefinition();

		// Namespaces
		static AstNode* ParseNamespaceName();
		static AstNode* ParseNamespaceDefinition();
		static AstNode* ParseNamedNamespaceDefinition();
		static AstNode* ParseUnnamedNamespaceDefinition();
		static AstNode* ParseNamespaceBody();

		// Namespace alias
		static AstNode* ParseNamespaceAliasDefinition();
		static AstNode* ParseQualifiedNamespaceSpecifier();

		// Using
		static AstNode* ParseUsingDeclaration();
		static AstNode* ParseUsingDirective();
		static AstNode* ParseAsmDefinition();
		static AstNode* ParseLinkageSpecification();

		// Declaration Grammar
		static AstNode* ParseAttributeSpecifierSequence();
		static AstNode* ParseAttributeSpecifier();
		static AstNode* ParseAlignmentSpecifier();
		static AstNode* ParseAttributeList();
		static AstNode* ParseAttribute();
		static AstNode* ParseAttributeToken();
		static AstNode* ParseAttributeArgumentClause();
		static AstNode* ParseBalancedTokenSequence();
		static AstNode* ParseBalancedToken();

		// Declarations
		static AstNode* ParseInitDeclaratorList();
		static AstNode* ParseInitDeclarator();
		static AstNode* ParseDeclarator();
		static AstNode* ParsePtrDeclarator();
		static AstNode* ParseNoPtrDeclarator();
		static AstNode* ParseParametersAndQualifiers();
		static AstNode* ParseTrailingReturnType();
		static AstNode* ParsePtrOperator();
		static AstNode* ParseCvQualifierSequence();
		static AstNode* ParseCvQualifier();
		static AstNode* ParseRefQualifier();
		static AstNode* ParseDeclaratorId();

		// dcl.name
		static AstNode* ParseTypeId();
		static AstNode* ParseAbstractDeclarator();
		static AstNode* ParsePtrAbstractDeclarator();
		static AstNode* ParseNoptrAbstractDeclarator();

		// dcl.fct
		static AstNode* ParseParameterDeclarationClause();
		static AstNode* ParseParameterDeclarationList();
		static AstNode* ParseParameterDeclaration();

		// Functions
		static AstNode* ParseFunctionDefinition();
		static AstNode* ParseFunctionBody();

		// Init
		static AstNode* ParseInitializer();
		static AstNode* ParseBraceOrEqualInitializer();
		static AstNode* ParseInitializerClause();
		static AstNode* ParseInitializerList();
		static AstNode* ParseBracedInitList();

		// Classes
		static AstNode* ParseClassName();
		static AstNode* ParseClassSpecifier();
		static AstNode* ParseClassHead();
		static AstNode* ParseClassHeadName();
		static AstNode* ParseClassVirtSpecifierSequence();
		static AstNode* ParseClassVirtSpecifier();
		static AstNode* ParseClassKey();

		// Class Members
		static AstNode* ParseMemberSpecification();
		static AstNode* ParseMemberDeclaration();
		static AstNode* ParseMemberDeclaratorList();
		static AstNode* ParseMemberDeclarator();
		static AstNode* ParseVirtSpecifierSequence();
		static AstNode* ParseVirtSpecifier();
		static AstNode* ParsePureSpecifier();

		// Derived classes
		static AstNode* ParseBaseClause();
		static AstNode* ParseBaseSpecifierList();
		static AstNode* ParseBaseSpecifier();
		static AstNode* ParseClassOrDecltype();
		static AstNode* ParseBaseTypeSpecifier();
		static AstNode* ParseAccessSpecifier();

		// Class conversion functions
		static AstNode* ParseConversionFunctionId();
		static AstNode* ParseConversionTypeId();
		static AstNode* ParseConversionDeclarator();

		// Class initializers
		static AstNode* ParseCtorInitializer();
		static AstNode* ParseMemInitializerList();
		static AstNode* ParseMemInitializer();
		static AstNode* ParseMemInitializerId();

		// Operator overloading
		static AstNode* ParseOperatorFunctionId();
		static OverloadableOperatorType ParseOverloadableOperator();

		// Literal overrides
		static AstNode* ParseLiteralOperatorId();

		// Templates
		static AstNode* ParseTemplateDeclaration();
		static AstNode* ParseTemplateParameterList();
		static AstNode* ParseTemplateParameter();
		static AstNode* ParseTypeParameter();
		static AstNode* ParseSimpleTemplateId();
		static AstNode* ParseTemplateId();
		static AstNode* ParseTemplateName();
		static AstNode* ParseTemplateArgumentList();
		static AstNode* ParseTemplateArgument();

		static AstNode* ParseTypenameSpecifier();
		static AstNode* ParseExplicitInstantiation();
		static AstNode* ParseExplicitSpecialization();

		// Exceptions
		static AstNode* ParseTryBlock();
		static AstNode* ParseFunctionTryBlock();
		static AstNode* ParseHandlerSequence();
		static AstNode* ParseHandler();
		static AstNode* ParseExceptionDeclaration();

		static AstNode* ParseThrowExpression();
		static AstNode* ParseExceptionSpecification();
		static AstNode* ParseDynamicExceptionSpecification();
		static AstNode* ParseTypeIdList();
		static AstNode* ParseNoexceptSpecification();

		// Preprocessor File
		// TODO: Add this stuff in once I write a preprocessing engine
		//static AstNode* ParsePreprocessingFile();
		//static AstNode* ParseGroup();
		//static AstNode* ParseGroupPart();
		//static AstNode* ParseIfSection();
		//static AstNode* ParseIfGroup();
		//static AstNode* ParseElifGroups();
		//static AstNode* ParseElifGroup();
		//static AstNode* ParseElseGroup();
		//static AstNode* ParseControlLine();
		//static AstNode* ParseTextLine();
		//static AstNode* ParseNonDirective();
		//static AstNode* ParseIdentifierList();
		//static AstNode* ParseReplacementList();
		//static AstNode* ParsePPTokens();
		//static AstNode* ParseNumberLiteral();

		//// Preprocessor Stuff
		//static AstNode* ParsePreprocessingToken();
		//static AstNode* ParseHeaderName();
		//static AstNode* ParseCharacterLiteral();
		//static AstNode* ParseUserDefinedCharacterLiteral();
		//static AstNode* ParseStringLiteral();
		//static AstNode* ParseUserDefinedStringLiteral();
		//static AstNode* ParsePreprocessingOpOrPunc();
		//static AstNode* ParseHCharSequence();
		//static AstNode* ParseHChar();
		//static AstNode* ParseQCharSequence();
		//static AstNode* ParseQChar();

		// Implementation -----------------------------------------------------------------------------------------------------------------------------------------
		// Translation Unit
		static AstNode* ParseTranslationUnit()
		{
			return ParseDeclarationSequence();
		}

		// Expressions
		static AstNode* ParsePrimaryExpression()
		{
			if (Peek() == TokenType::CHARACTER_LITERAL || Peek() == TokenType::FLOATING_POINT_LITERAL || Peek() == TokenType::INTEGER_LITERAL || Peek() == TokenType::STRING_LITERAL)
			{
				return GenerateLiteralNode(ConsumeCurrent(Peek()));
			}

			if (Peek() == TokenType::KW_THIS)
			{
				return GenerateThisNode(ConsumeCurrent(Peek()));
			}

			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GenerateGroupingNode(expression);
			}

			int backtrackPosition = CurrentToken;
			AstNode* expr = ParseIdExpression();
			if (expr->success)
			{
				return expr;
			}
			FreeNode(expr);
			BacktrackTo(backtrackPosition);

			AstNode* lambdaExpr = ParseLambdaExpression();
			if (lambdaExpr->success)
			{
				return lambdaExpr;
			}
			FreeNode(expr);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseIdExpression()
		{
			int backtrackPosition = CurrentToken;
			AstNode* unqualifiedId = ParseUnqualifiedId();
			if (unqualifiedId->success)
			{
				return unqualifiedId;
			}
			FreeNode(unqualifiedId);
			BacktrackTo(backtrackPosition);

			AstNode* qualifiedId = ParseQualifiedId();
			if (qualifiedId->success)
			{
				return qualifiedId;
			}
			FreeNode(qualifiedId);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUnqualifiedId()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateUnqualifiedIdNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			int backtrackPosition = CurrentToken;
			if (Match(TokenType::TILDE))
			{
				AstNode* className = ParseClassName();
				if (className->success)
				{
					return GenerateUnqualifiedIdDtorClassNode(className);
				}
				FreeNode(className);
				BacktrackTo(backtrackPosition);

				Consume(TokenType::TILDE);
				AstNode* decltypeSpecifier = ParseDecltypeSpecifier();
				if (decltypeSpecifier->success)
				{
					return GenerateUnqualifiedIdDtorDecltypeNode(decltypeSpecifier);
				}
				FreeNode(decltypeSpecifier);
			}
			BacktrackTo(backtrackPosition);

			AstNode* operatorFunctionId = ParseOperatorFunctionId();
			if (operatorFunctionId->success)
			{
				return operatorFunctionId;
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(backtrackPosition);

			AstNode* conversionFunctionId = ParseConversionFunctionId();
			if (conversionFunctionId->success)
			{
				return conversionFunctionId;
			}
			FreeNode(conversionFunctionId);
			BacktrackTo(backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId();
			if (literalOperatorId->success)
			{
				return literalOperatorId;
			}
			FreeNode(literalOperatorId);
			BacktrackTo(backtrackPosition);

			AstNode* templateId = ParseTemplateId();
			if (templateId->success)
			{
				return templateId;
			}
			FreeNode(templateId);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseQualifiedId()
		{
			int backtrackPosition = CurrentToken;
			bool hasNamespaceScope = Peek() == TokenType::COLON;
			if (Peek() == TokenType::COLON)
			{
				Consume(TokenType::COLON);
				Consume(TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			bool hasTemplateKeyword = Match(TokenType::KW_TEMPLATE);
			AstNode* unqualifiedId = ParseUnqualifiedId();
			if (!hasNamespaceScope)
			{
				// This can only be a nested-namespace-specifier if it doesn't have a namespace scope
				if (nestedNameSpecifier->success && unqualifiedId->success)
				{
					return GenerateTemplateQualifiedIdNode(nestedNameSpecifier, hasNamespaceScope, hasTemplateKeyword);
				}
				FreeNode(nestedNameSpecifier);
				FreeNode(unqualifiedId);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (nestedNameSpecifier->success && unqualifiedId->success)
			{
				return GenerateTemplateQualifiedIdNode(nestedNameSpecifier, hasNamespaceScope, hasTemplateKeyword);
			}
			FreeNode(nestedNameSpecifier);
			FreeNode(unqualifiedId);
			BacktrackTo(backtrackPosition);

			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateQualifiedIdNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			AstNode* operatorFunctionId = ParseOperatorFunctionId();
			if (operatorFunctionId->success)
			{
				return operatorFunctionId;
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId();
			if (literalOperatorId->success)
			{
				return literalOperatorId;
			}
			FreeNode(literalOperatorId);
			BacktrackTo(backtrackPosition);

			AstNode* templateId = ParseTemplateId();
			if (templateId->success)
			{
				return templateId;
			}
			FreeNode(templateId);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNestedNameSubSpecifier()
		{
			int backtrackPosition = CurrentToken;
			AstNode* typeName = ParseTypeName();
			if (typeName->success)
			{
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
					return typeName;
				}
			}
			FreeNode(typeName);
			BacktrackTo(backtrackPosition);

			AstNode* namespaceName = ParseNamespaceName();
			if (namespaceName->success)
			{
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
					return namespaceName;
				}
			}
			FreeNode(namespaceName);
			BacktrackTo(backtrackPosition);

			AstNode* decltypeSpecifier = ParseDecltypeSpecifier();
			if (decltypeSpecifier->success)
			{
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
					return decltypeSpecifier;
				}
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNestedNameSpecifier()
		{
			int backtrackPosition = CurrentToken;
			AstNode* nestedNameSubSpecifier = ParseNestedNameSubSpecifier();
			if (nestedNameSubSpecifier->success)
			{
				return nestedNameSubSpecifier;
			}
			FreeNode(nestedNameSubSpecifier);
			BacktrackTo(backtrackPosition);

			// If the next token is not an identifier, or a template OR there are no more semicolons, then return no success
			if (!PeekIn({ TokenType::IDENTIFIER, TokenType::KW_TEMPLATE }) || !MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				return GenerateNoSuccessAstNode();
			}
			AstNode* nestedNameSpecifier = ParseNestedNameSpecifier();
			if (Peek() == TokenType::IDENTIFIER)
			{
				Token id = ConsumeCurrent(TokenType::IDENTIFIER);
				Consume(TokenType::COLON);
				Consume(TokenType::COLON);
				return GenerateNestedNamespaceSpecifierIdNode(nestedNameSpecifier, id);
			}

			bool hasTemplateKeyword = Match(TokenType::KW_TEMPLATE);
			AstNode* simpleTemplateId = ParseSimpleTemplateId();
			if (simpleTemplateId->success)
			{
				Consume(TokenType::COLON);
				Consume(TokenType::COLON);
				return GenerateNestedNamespaceSpecifierTemplateNode(nestedNameSpecifier, hasTemplateKeyword, simpleTemplateId);
			}
			FreeNode(nestedNameSpecifier);
			FreeNode(simpleTemplateId);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Lambdas
		static AstNode* ParseLambdaExpression()
		{
			int backtrackPosition = CurrentToken;
			AstNode* lambdaIntroducer = ParseLambdaIntroducer();
			if (!lambdaIntroducer->success)
			{
				FreeNode(lambdaIntroducer);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// This is optional, so it's fine if it doesn't succeed
			backtrackPosition = CurrentToken;
			AstNode* lambdaDeclarator = ParseLambdaDeclarator();
			if (!lambdaDeclarator->success)
			{
				BacktrackTo(backtrackPosition);
			}

			backtrackPosition = CurrentToken;
			AstNode* compoundStatement = ParseCompoundStatement();
			if (!compoundStatement->success)
			{
				FreeNode(lambdaIntroducer);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateLambdaExpressionNode(lambdaIntroducer, lambdaDeclarator, compoundStatement);
		}

		static AstNode* ParseLambdaIntroducer()
		{
			if (Match(TokenType::LEFT_BRACKET))
			{
				int backtrackPosition = CurrentToken;
				// Lambda capture is optional, so it's ok if it fails
				AstNode* lambdaCapture = ParseLambdaCapture();
				if (!lambdaCapture->success)
				{
					BacktrackTo(backtrackPosition);
				}
				Consume(TokenType::RIGHT_BRACKET);

				return GenerateLambdaIntroducerNode(lambdaCapture);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLambdaCapture()
		{
			Token captureDefault;
			captureDefault.m_Type = TokenType::None;
			if (Peek() == TokenType::AND || Peek() == TokenType::EQUAL)
			{
				captureDefault = ConsumeCurrent(Peek());
			}

			if (captureDefault.m_Type != TokenType::None && Match(TokenType::COMMA))
			{
				return GenerateLambdaCaptureNode(captureDefault, ParseCaptureList());
			}

			if (captureDefault.m_Type == TokenType::None)
			{
				return GenerateLambdaCaptureNode(captureDefault, ParseCaptureList());
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCaptureList()
		{
			AstNode* result = ParseCapture();

			while (Match(TokenType::COMMA))
			{
				result = GenerateLambdaCaptureListNode(result, ParseCaptureList());
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return GenerateLambdaCaptureListNode(result, nullptr);
		}

		static AstNode* ParseCapture()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::AND))
			{
				if (Peek() == TokenType::IDENTIFIER)
				{
					return GenerateCaptureNode(ConsumeCurrent(TokenType::IDENTIFIER));
				}

				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Peek() == TokenType::KW_THIS)
			{
				return GenerateThisNode(ConsumeCurrent(TokenType::KW_THIS));
			}

			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateCaptureNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLambdaDeclarator()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause();
				Consume(TokenType::RIGHT_PAREN);

				bool isMutable = Match(TokenType::KW_MUTABLE);

				int backtrackPosition = CurrentToken;
				AstNode* exceptionSpec = ParseExceptionSpecification();
				if (!exceptionSpec->success)
				{
					FreeNode(exceptionSpec);
					exceptionSpec = GenerateNoSuccessAstNode();
					BacktrackTo(backtrackPosition);
				}

				backtrackPosition = CurrentToken;
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				if (!attributeSpecifierSeq->success)
				{
					FreeNode(attributeSpecifierSeq);
					attributeSpecifierSeq = GenerateNoSuccessAstNode();
					BacktrackTo(backtrackPosition);
				}

				backtrackPosition = CurrentToken;
				AstNode* trailingReturnType = ParseTrailingReturnType();
				if (!trailingReturnType->success)
				{
					FreeNode(trailingReturnType);
					trailingReturnType = GenerateNoSuccessAstNode();
					BacktrackTo(backtrackPosition);
				}

				return GenerateLambdaDeclaratorNode(parameterDeclarationClause, isMutable, exceptionSpec, attributeSpecifierSeq, trailingReturnType);
			}

			return GenerateNoSuccessAstNode();
		}

		// Postfix Expressions
		static AstNode* ParsePostfixExpression()
		{
			// TODO: This one scares me test pretty good, otherwise I have a feeling you will get infinite loops which results in horrible lag
			// Try to look ahead and make sure we don't recurse further if it's not possible
			bool shouldRecurse = LookAheadBeforeSemicolon({ TokenType::LEFT_BRACKET, TokenType::LEFT_PAREN, TokenType::DOT, TokenType::ARROW, TokenType::PLUS_PLUS, TokenType::MINUS_MINUS });

			int backtrackPosition = CurrentToken;
			AstNode* primaryExpression = ParsePrimaryExpression();
			if (primaryExpression->success)
			{
				return primaryExpression;
			}
			FreeNode(primaryExpression);
			BacktrackTo(backtrackPosition);

			AstNode* simpleTypeSpecifier = ParseSimpleTypeSpecifier();
			if (simpleTypeSpecifier->success)
			{
				if (Match(TokenType::LEFT_PAREN))
				{
					// Optional
					AstNode* expressionList = ParseExpressionList();
					Consume(TokenType::RIGHT_PAREN);
					return GeneratePostfixSimpleTypeExpressionListNode(simpleTypeSpecifier, expressionList);
				}

				AstNode* bracedInitList = ParseBracedInitList();
				if (!bracedInitList->success)
				{
					FreeNode(simpleTypeSpecifier);
					FreeNode(bracedInitList);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				return GeneratePostfixSimpleTypeBraceListNode(simpleTypeSpecifier, bracedInitList);
			}
			FreeNode(simpleTypeSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* typenameSpecifier = ParseTypenameSpecifier();
			if (typenameSpecifier->success)
			{
				if (Match(TokenType::LEFT_PAREN))
				{
					// Optional
					AstNode* expressionList = ParseExpressionList();
					Consume(TokenType::RIGHT_PAREN);
					return GeneratePostfixTypenameSpecExpressionListNode(simpleTypeSpecifier, expressionList);
				}

				AstNode* bracedInitList = ParseBracedInitList();
				if (!bracedInitList->success)
				{
					FreeNode(simpleTypeSpecifier);
					FreeNode(bracedInitList);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				return GeneratePostfixTypenameSpecBraceListNode(simpleTypeSpecifier, bracedInitList);
			}
			FreeNode(typenameSpecifier);
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::KW_DYNAMIC_CAST))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::DynamicCast);
			}

			if (Match(TokenType::KW_STATIC_CAST))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::StaticCast);
			}

			if (Match(TokenType::KW_REINTERPRET_CAST))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::ReinterpretCast);
			}

			if (Match(TokenType::KW_CONST_CAST))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				AstNode* typeId = ParseTypeId();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GeneratePostfixCastNode(typeId, expression, CastType::ConstCast);
			}

			if (Match(TokenType::KW_TYPEID))
			{
				Consume(TokenType::LEFT_PAREN);
				int backtrackPos2 = CurrentToken;
				AstNode* expression = ParseExpression();
				if (expression->success)
				{
					Consume(TokenType::RIGHT_PAREN);
					return GeneratePostfixTypeIdExpressionNode(expression);
				}
				FreeNode(expression);
				BacktrackTo(backtrackPos2);

				AstNode* typeId = ParseTypeId();
				if (typeId->success)
				{
					Consume(TokenType::RIGHT_PAREN);
					return GeneratePostfixTypeIdNode(typeId);
				}
				FreeNode(typeId);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (!shouldRecurse)
			{
				return GenerateNoSuccessAstNode();
			}

			AstNode* postfixExpression = ParsePostfixExpression();
			if (Match(TokenType::LEFT_BRACKET))
			{
				int backtrackPosition2 = CurrentToken;
				AstNode* expression = ParseExpression();
				if (expression->success)
				{
					Consume(TokenType::RIGHT_BRACKET);
					return GeneratePostfixBracketExpressionNode(postfixExpression, expression);
				}
				FreeNode(expression);
				BacktrackTo(backtrackPosition2);

				// Optional
				AstNode* bracedInitList = ParseBracedInitList();
				Consume(TokenType::RIGHT_BRACKET);
				return GeneratePostfixBracketBraceListNode(postfixExpression, bracedInitList);
			}

			if (Match(TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList();
				Consume(TokenType::RIGHT_PAREN);
				return GeneratePostfixParenExpressionListNode(postfixExpression, expressionList);
			}

			bool isDot = Match(TokenType::DOT);
			bool isArrow = Match(TokenType::ARROW);
			if (isDot || isArrow)
			{
				MemberOperatorType memberOp = isDot ? MemberOperatorType::DotOperator : MemberOperatorType::ArrowOperator;
				bool hasTemplateKeyword = Match(TokenType::KW_TEMPLATE);
				if (hasTemplateKeyword)
				{
					AstNode* idExpression = ParseIdExpression();
					if (!idExpression->success)
					{
						FreeNode(postfixExpression);
						return GenerateNoSuccessAstNode();
					}
					return GeneratePostfixMemberIdExpressionNode(postfixExpression, idExpression, hasTemplateKeyword, memberOp);
				}

				int backtrackPosition2 = CurrentToken;
				AstNode* idExpression = ParseIdExpression();
				if (idExpression->success)
				{
					return GeneratePostfixMemberIdExpressionNode(postfixExpression, idExpression, hasTemplateKeyword, memberOp);
				}
				FreeNode(idExpression);
				BacktrackTo(backtrackPosition2);

				AstNode* pseudoDestructorName = ParsePseudoDestructorName();
				return GeneratePostfixPseudoDestructorNode(postfixExpression, pseudoDestructorName, memberOp);
			}

			if (Match(TokenType::PLUS_PLUS))
			{
				return GeneratePostfixPlusPlusNode(postfixExpression);
			}

			if (Match(TokenType::MINUS_MINUS))
			{
				return GeneratePostfixMinusMinusNode(postfixExpression);
			}

			FreeNode(postfixExpression);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpressionList()
		{
			return ParseInitializerList();
		}

		static AstNode* ParsePseudoDestructorName()
		{
			if (Match(TokenType::TILDE))
			{
				AstNode* decltypeSpecifier = ParseDecltypeSpecifier();
				return GeneratePseudoDestructorDecltypeNode(decltypeSpecifier);
			}

			int backtrackPosition = CurrentToken;
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			if (Match(TokenType::KW_TEMPLATE))
			{
				if (!nestedNameSpecifier->success)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				AstNode* simpleTemplateId = ParseSimpleTemplateId();
				Consume(TokenType::COLON);
				Consume(TokenType::COLON);
				Consume(TokenType::TILDE);
				AstNode* typeName = ParseTypeName();
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(simpleTemplateId);
					FreeNode(typeName);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoDestructorTemplateNode(nestedNameSpecifier, simpleTemplateId, typeName);
			}

			// Nested name specifier is optional at this point
			if (Match(TokenType::TILDE))
			{
				AstNode* typeName = ParseTypeName();
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(typeName);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoDestructorNode(nestedNameSpecifier, typeName);
			}

			AstNode* nestedTypeName = ParseTypeName();
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
				Consume(TokenType::TILDE);
				AstNode* typeName = ParseTypeName();
				if (!typeName->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(nestedTypeName);
					FreeNode(typeName);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GeneratePseudoNestedDestructorNode(nestedNameSpecifier, nestedTypeName, typeName);
			}

			FreeNode(nestedTypeName);
			FreeNode(nestedNameSpecifier);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Unary Expressions
		static AstNode* ParseUnaryExpression()
		{
			int backtrackCursor = CurrentToken;
			AstNode* postfix = ParsePostfixExpression();
			if (postfix->success)
			{
				return postfix;
			}
			FreeNode(postfix);
			BacktrackTo(backtrackCursor);

			if (Peek() == TokenType::PLUS_PLUS || Peek() == TokenType::MINUS_MINUS || Peek() == TokenType::STAR || Peek() == TokenType::AND ||
				Peek() == TokenType::PLUS || Peek() == TokenType::MINUS || Peek() == TokenType::BANG || Peek() == TokenType::TILDE)
			{
				return GenerateUnaryExpressionNode(ParseOverloadableOperator(), ParseCastExpression());
			}

			if (Match(TokenType::KW_SIZEOF))
			{
				if (Match(TokenType::LEFT_PAREN))
				{
					AstNode* typeId = ParseTypeId();
					Consume(TokenType::RIGHT_PAREN);
					return GenerateSizeofExpressionNode(typeId);
				}

				if (Match(TokenType::DOT))
				{
					Consume(TokenType::DOT);
					Consume(TokenType::DOT);
					Consume(TokenType::LEFT_PAREN);
					Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
					Consume(TokenType::RIGHT_PAREN);
					return GenerateSizeofIdentifierExpressionNode(identifier);
				}

				return GenerateSizeofExpressionNode(ParseUnaryExpression());
			}

			AstNode* alignmentExpression = ParseAlignmentExpression();
			if (alignmentExpression->success)
			{
				return alignmentExpression;
			}
			FreeNode(alignmentExpression);
			BacktrackTo(backtrackCursor);

			AstNode* noexceptExpr = ParseNoexceptExpression();
			if (noexceptExpr->success)
			{
				return noexceptExpr;
			}
			FreeNode(noexceptExpr);
			BacktrackTo(backtrackCursor);

			AstNode* newExpr = ParseNewExpression();
			if (newExpr->success)
			{
				return newExpr;
			}
			FreeNode(newExpr);
			BacktrackTo(backtrackCursor);

			AstNode* deleteExpr = ParseDeleteExpression();
			if (deleteExpr->success)
			{
				return deleteExpr;
			}
			FreeNode(deleteExpr);
			BacktrackTo(backtrackCursor);

			return GenerateNoSuccessAstNode();
		}

		// New Expressions
		static AstNode* ParseNewExpression()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
			}

			if (Match(TokenType::KW_NEW))
			{
				// Optional
				AstNode* newPlacement = ParseNewPlacement();
				if (Match(TokenType::LEFT_PAREN))
				{
					AstNode* typeId = ParseTypeId();
					Consume(TokenType::RIGHT_PAREN);

					// Optional
					AstNode* newInitializer = ParseNewInitializer();
					return GenerateNewExpressionNode(newPlacement, typeId, newInitializer);
				}

				AstNode* newTypeId = ParseNewTypeId();
				if (!newTypeId->success)
				{
					FreeNode(newPlacement);
					FreeNode(newTypeId);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				// Optional
				AstNode* newInitializer = ParseNewInitializer();
				return GenerateNewTypeIdExpressionNode(newPlacement, newTypeId, newInitializer);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewPlacement()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* expressionList = ParseExpressionList();
				if (!expressionList->success)
				{
					FreeNode(expressionList);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				Consume(TokenType::RIGHT_PAREN);

				return GenerateNewPlacementNode(expressionList);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewTypeId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence();
			if (!typeSpecifierSeq->success)
			{
				FreeNode(typeSpecifierSeq);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* newDeclarator = ParseNewDeclarator();
			return GenerateNewTypeIdNode(typeSpecifierSeq, newDeclarator);
		}

		static AstNode* ParseNewDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* noptrNewDeclarator = ParseNoptrNewDeclarator();
			if (noptrNewDeclarator->success)
			{
				return noptrNewDeclarator;
			}
			FreeNode(noptrNewDeclarator);
			BacktrackTo(backtrackPosition);

			AstNode* ptrOperator = ParsePtrOperator();
			if (!ptrOperator->success)
			{
				FreeNode(ptrOperator);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* newDeclarator = ParseNewDeclarator();
			return GenerateNewDeclaratorNode(ptrOperator, newDeclarator);
		}

		static AstNode* ParseNoptrNewDeclarator()
		{
			// TODO: this recursion is crazy
			int backtrackPosition = CurrentToken;
			if (!Match(TokenType::LEFT_BRACKET))
			{
				return GenerateNoSuccessAstNode();
			}

			AstNode* expression = ParseExpression();
			if (expression->success)
			{
				Consume(TokenType::RIGHT_BRACKET);

				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				return GenerateNoptrNewTailDeclaratorNode(expression, attributeSpecifierSeq);
			}
			FreeNode(expression);
			BacktrackTo(backtrackPosition);

			Consume(TokenType::LEFT_BRACKET);
			AstNode* constantExpression = ParseConstantExpression();
			if (constantExpression->success)
			{
				Consume(TokenType::RIGHT_BRACKET);
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();

				AstNode* noptrNewDeclarator = ParseNoptrNewDeclarator();
				if (!noptrNewDeclarator->success)
				{
					FreeNode(noptrNewDeclarator);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateNoptrNewDeclaratorNode(noptrNewDeclarator, constantExpression, attributeSpecifierSeq);
			}

			FreeNode(constantExpression);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNewInitializer()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList();
				Consume(TokenType::RIGHT_PAREN);

				return GenerateNewInitializerNode(expressionList);
			}

			return ParseBracedInitList();
		}

		// Delete 
		static AstNode* ParseDeleteExpression()
		{
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
			}

			if (Match(TokenType::KW_DELETE))
			{
				bool deleteArr = false;
				if (Match(TokenType::LEFT_BRACKET))
				{
					Consume(TokenType::RIGHT_BRACKET);
					deleteArr = true;
				}

				return GenerateDeleteNode(ParseCastExpression(), deleteArr);
			}

			return GenerateNoSuccessAstNode();
		}

		// Noexcept
		static AstNode* ParseNoexceptExpression()
		{
			if (Match(TokenType::KW_NOEXCEPT))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return expression;
			}

			return GenerateNoSuccessAstNode();
		}

		// Cast
		static AstNode* ParseCastExpression()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* typeId = ParseTypeId();
				Consume(TokenType::RIGHT_PAREN);
				return GenerateCastExpressionNode(typeId, ParseCastExpression());
			}

			return ParseUnaryExpression();
		}

		// PointerToMember Expression
		static AstNode* ParsePmExpression()
		{
			AstNode* result = ParseCastExpression();

			// TODO: Does it matter that I'm doing left recursion and he does right recursion????
			while (Match(TokenType::POINTER_TO_MEMBER))
			{
				AstNode* left = ParsePmExpression();
				result = GeneratePointerToMemberNode(left, result);
			}

			return result;
		}

		// Primary operations
		static AstNode* ParseMultiplicativeExpression()
		{
			AstNode* result = ParsePmExpression();

			while (Peek() == TokenType::STAR || Peek() == TokenType::DIV || Peek() == TokenType::MODULO)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseMultiplicativeExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseAdditiveExpression()
		{
			AstNode* result = ParseMultiplicativeExpression();

			while (Peek() == TokenType::PLUS || Peek() == TokenType::MINUS)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseAdditiveExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseShiftExpression()
		{
			AstNode* result = ParseAdditiveExpression();

			while (Peek() == TokenType::LEFT_SHIFT || Peek() == TokenType::RIGHT_SHIFT)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseShiftExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Comparision operations
		static AstNode* ParseRelationalExpression()
		{
			AstNode* result = ParseShiftExpression();

			while (Peek() == TokenType::LEFT_ANGLE_BRACKET || Peek() == TokenType::RIGHT_ANGLE_BRACKET || Peek() == TokenType::LESS_THAN_EQ || Peek() == TokenType::GREATER_THAN_EQ)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseRelationalExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseEqualityExpression()
		{
			AstNode* result = ParseRelationalExpression();

			while (Peek() == TokenType::EQUAL_EQUAL || Peek() == TokenType::BANG_EQUAL)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseEqualityExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Logical operations
		static AstNode* ParseAndExpression()
		{
			AstNode* result = ParseEqualityExpression();

			while (Peek() == TokenType::AND)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseAndExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseExclusiveOrExpression()
		{
			AstNode* result = ParseAndExpression();

			while (Peek() == TokenType::CARET)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseExclusiveOrExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseInclusiveOrExpression()
		{
			AstNode* result = ParseExclusiveOrExpression();

			while (Peek() == TokenType::PIPE)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseInclusiveOrExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseLogicalAndExpression()
		{
			AstNode* result = ParseInclusiveOrExpression();

			while (Peek() == TokenType::LOGICAL_AND)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseLogicalAndExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		static AstNode* ParseLogicalOrExpression()
		{
			AstNode* result = ParseLogicalAndExpression();

			while (Peek() == TokenType::LOGICAL_OR)
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				AstNode* right = ParseLogicalOrExpression();
				result = GenerateBinaryExpressionNode(result, op, right);
			}

			return result;
		}

		// Misc expressions
		static AstNode* ParseConditionalExpression()
		{
			AstNode* result = ParseLogicalOrExpression();

			if (Match(TokenType::QUESTION))
			{
				AstNode* ifTrueNode = ParseExpression();
				Consume(TokenType::SEMICOLON);
				AstNode* ifFalseNode = ParseAssignmentExpression();
				result = GenerateTernaryExpressionNode(result, ifTrueNode, ifFalseNode);
			}

			return result;
		}

		static AstNode* ParseAssignmentExpression()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseConditionalExpression();

			if (result->success)
			{
				return result;
			}
			FreeNode(result);
			BacktrackTo(backtrackPosition);

			result = ParseLogicalOrExpression();
			if (IsAssignmentOperator(Peek()))
			{
				AssignmentOperatorType assignmentType = AssignmentOperatorType::None;
				switch (Peek())
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
				result = GenerateAssignmentExpressionNode(result, assignmentType, ParseInitializerClause());
				return result;
			}
			FreeNode(result);
			BacktrackTo(backtrackPosition);

			return ParseThrowExpression();
		}

		static AstNode* ParseAlignmentExpression()
		{
			int backtrackPosition = CurrentToken;
			if (Peek() == TokenType::KW_ALIGN_OF)
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* typeId = ParseTypeId();
				if (typeId->success)
				{
					Consume(TokenType::RIGHT_PAREN);
					return GenerateAlignmentExpressionNode(typeId);
				}
				FreeNode(typeId);
				BacktrackTo(backtrackPosition);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpression()
		{
			AstNode* expression = ParseAssignmentExpression();

			while (Match(TokenType::COMMA))
			{
				AstNode* nextExpression = expression;
				AstNode* expression = GenerateAstNode(AstNodeType::Expression);
				expression->expressionNode.expression = ParseExpression();
				expression->expressionNode.nextExpression = nextExpression;
			}

			return expression;
		}

		static AstNode* ParseConstantExpression()
		{
			return GenerateConstantExpressionNode(ParseConditionalExpression());
		}

		// Statements
		static AstNode* ParseStatement()
		{
			int backtrackPosition = CurrentToken;
			AstNode* labeledStatement = ParseLabeledStatement();
			if (labeledStatement->success)
			{
				return GenerateStatementNode(GenerateNoSuccessAstNode(), labeledStatement);
			}
			FreeNode(labeledStatement);
			BacktrackTo(backtrackPosition);

			AstNode* declarationStatement = ParseDeclarationStatement();
			if (declarationStatement->success)
			{
				return GenerateStatementNode(GenerateNoSuccessAstNode(), declarationStatement);
			}
			FreeNode(declarationStatement);
			BacktrackTo(backtrackPosition);

			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			int backtrackPosition2 = CurrentToken;

			AstNode* expressionStatement = ParseExpressionStatement();
			if (expressionStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, expressionStatement);
			}
			FreeNode(expressionStatement);
			BacktrackTo(backtrackPosition2);

			AstNode* compoundStatement = ParseCompoundStatement();
			if (compoundStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, compoundStatement);
			}
			FreeNode(compoundStatement);
			BacktrackTo(backtrackPosition2);

			AstNode* selectionStatement = ParseSelectionStatement();
			if (selectionStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, selectionStatement);
			}
			FreeNode(selectionStatement);
			BacktrackTo(backtrackPosition2);

			AstNode* iterationStatement = ParseIterationStatement();
			if (iterationStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, iterationStatement);
			}
			FreeNode(iterationStatement);
			BacktrackTo(backtrackPosition2);

			AstNode* jumpStatement = ParseJumpStatement();
			if (jumpStatement->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, jumpStatement);
			}
			FreeNode(jumpStatement);
			BacktrackTo(backtrackPosition2);

			AstNode* tryBlock = ParseTryBlock();
			if (tryBlock->success)
			{
				return GenerateStatementNode(attributeSpecifierSeq, tryBlock);
			}
			FreeNode(tryBlock);
			FreeNode(attributeSpecifierSeq);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLabeledStatement()
		{
			int backtrackPosition = CurrentToken;
			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();

			if (Peek() == TokenType::IDENTIFIER)
			{
				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				Consume(TokenType::COLON);
				AstNode* statement = ParseStatement();
				return GenerateLabeledIdentifierNode(attributeSpecifierSeq, identifier, statement);
			}

			if (Match(TokenType::KW_CASE))
			{
				AstNode* constantExpression = ParseConstantExpression();
				Consume(TokenType::COLON);
				AstNode* statement = ParseStatement();
				return GenerateCaseLabelNode(attributeSpecifierSeq, constantExpression, statement);
			}

			if (Match(TokenType::KW_DEFAULT))
			{
				Consume(TokenType::COLON);
				AstNode* statement = ParseStatement();
				return GenerateDefaultLabelNode(attributeSpecifierSeq, statement);
			}

			FreeNode(attributeSpecifierSeq);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExpressionStatement()
		{
			if (Match(TokenType::SEMICOLON))
			{
				return GenerateEmptyStatementNode();
			}

			int backtrackPosition = CurrentToken;
			AstNode* expression = ParseExpression();
			if (expression->success)
			{
				Consume(TokenType::SEMICOLON);
				return expression;
			}

			FreeNode(expression);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCompoundStatement()
		{
			if (Match(TokenType::LEFT_BRACKET))
			{
				// Optional
				AstNode* statementSequence = ParseStatementSequence();
				Consume(TokenType::RIGHT_BRACKET);
				return GenerateCompoundStatementNode(statementSequence);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseStatementSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseStatement();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextStatement = nullptr;
			do
			{
				backtrackPosition = CurrentToken;
				AstNode* nextStatement = ParseStatement();
				result = GenerateStatementSequenceNode(result, nextStatement->success ? nextStatement : GenerateNoSuccessAstNode());
			} while (nextStatement && nextStatement->success);

			CPP_PARSER_LOG_ASSERT(nextStatement != nullptr, "Something went horribly wrong when parsing a statement sequence.");
			FreeNode(nextStatement);
			BacktrackTo(backtrackPosition);
			return result;
		}

		// Selection statements
		static AstNode* ParseSelectionStatement()
		{
			if (Match(TokenType::KW_IF))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition();
				Consume(TokenType::RIGHT_PAREN);

				AstNode* ifStatement = ParseStatement();
				AstNode* elseStatement = Match(TokenType::KW_ELSE) ?
					ParseStatement() :
					GenerateNoSuccessAstNode();

				return GenerateIfElseNode(condition, ifStatement, elseStatement);
			}

			if (Match(TokenType::KW_SWITCH))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition();
				Consume(TokenType::RIGHT_PAREN);

				AstNode* statement = ParseStatement();
				return GenerateSwitchNode(condition, statement);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCondition()
		{
			int backtrackPosition = CurrentToken;
			AstNode* expression = ParseExpression();
			if (expression->success)
			{
				return expression;
			}
			BacktrackTo(backtrackPosition);
			FreeNode(expression);

			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence();
			if (!declSpecifierSeq->success)
			{
				BacktrackTo(backtrackPosition);
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				return GenerateNoSuccessAstNode();
			}

			AstNode* declarator = ParseDeclarator();
			if (Match(TokenType::EQUAL))
			{
				AstNode* initializerClause = ParseInitializerClause();
				return GenerateInitializerConditionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, initializerClause);
			}

			AstNode* bracedInitList = ParseBracedInitList();
			return GenerateBracedInitConditionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, bracedInitList);
		}

		// Iteration statements
		static AstNode* ParseIterationStatement()
		{
			if (Match(TokenType::KW_WHILE))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* condition = ParseCondition();
				Consume(TokenType::RIGHT_PAREN);
				AstNode* statement = ParseStatement();

				return GenerateWhileLoopNode(condition, statement);
			}

			if (Match(TokenType::KW_DO))
			{
				AstNode* statement = ParseStatement();
				Consume(TokenType::KW_WHILE);
				Consume(TokenType::LEFT_PAREN);
				AstNode* condition = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				Consume(TokenType::SEMICOLON);

				return GenerateDoWhileLoopNode(statement, condition);
			}

			if (Match(TokenType::KW_FOR))
			{
				Consume(TokenType::LEFT_PAREN);
				int backtrackPosition = CurrentToken;
				AstNode* forInitStatement = ParseForInitStatement();
				if (forInitStatement->success)
				{
					// This is optional
					AstNode* condition = ParseCondition();
					Consume(TokenType::SEMICOLON);
					// This is also optional
					AstNode* expression = ParseExpression();
					Consume(TokenType::RIGHT_PAREN);
					AstNode* statement = ParseStatement();

					return GenerateForLoopNode(forInitStatement, condition, expression, statement);
				}
				FreeNode(forInitStatement);
				BacktrackTo(backtrackPosition);

				AstNode* forRangeDeclaration = ParseForRangeDeclaration();
				Consume(TokenType::COLON);
				AstNode* forRangeInitializer = ParseForRangeInitializer();
				Consume(TokenType::RIGHT_PAREN);
				AstNode* statement = ParseStatement();

				return GenerateForEachLoopNode(forRangeDeclaration, forRangeInitializer, statement);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseForInitStatement()
		{
			int backtrackPostion = CurrentToken;
			AstNode* expressionStatement = ParseExpressionStatement();
			if (expressionStatement->success)
			{
				return expressionStatement;
			}
			FreeNode(expressionStatement);
			BacktrackTo(backtrackPostion);

			AstNode* simpleDeclaration = ParseSimpleDeclaration();
			if (simpleDeclaration->success)
			{
				return simpleDeclaration;
			}

			FreeNode(simpleDeclaration);
			BacktrackTo(backtrackPostion);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseForRangeDeclaration()
		{
			int backtrackPostion = CurrentToken;
			// This is optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence();
			if (!typeSpecifierSeq->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(typeSpecifierSeq);
				BacktrackTo(backtrackPostion);
				return GenerateNoSuccessAstNode();
			}

			AstNode* declarator = ParseDeclarator();
			if (!declarator->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(typeSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(backtrackPostion);
				return GenerateNoSuccessAstNode();
			}

			return GenerateForRangeDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
		}

		static AstNode* ParseForRangeInitializer()
		{
			int backtrackPosition = CurrentToken;
			AstNode* expression = ParseExpression();
			if (!expression->success)
			{
				FreeNode(expression);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// TODO: For each loop seems weird do some good testing on this piece of grammar
			AstNode* bracedInitList = ParseBracedInitList();
			if (!bracedInitList->success)
			{
				FreeNode(expression);
				FreeNode(bracedInitList);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateForRangeInitializerNode(expression, bracedInitList);
		}

		// Jump statements
		static AstNode* ParseJumpStatement()
		{
			if (Match(TokenType::KW_BREAK))
			{
				Consume(TokenType::SEMICOLON);
				return GenerateBreakNode();
			}

			if (Match(TokenType::KW_CONTINUE))
			{
				Consume(TokenType::KW_CONTINUE);
				return GenerateContinueNode();
			}

			if (Match(TokenType::KW_RETURN))
			{
				if (Match(TokenType::SEMICOLON))
				{
					return GenerateReturnNode(GenerateNoSuccessAstNode());
				}

				int backtrackPosition = CurrentToken;
				AstNode* expression = ParseExpression();
				if (expression->success)
				{
					Consume(TokenType::SEMICOLON);
					return GenerateReturnNode(expression);
				}
				FreeNode(expression);
				BacktrackTo(backtrackPosition);

				AstNode* bracedInitList = ParseBracedInitList();
				if (bracedInitList->success)
				{
					Consume(TokenType::SEMICOLON);
					return GenerateReturnNode(bracedInitList);
				}
				FreeNode(bracedInitList);
				BacktrackTo(backtrackPosition);

				return GenerateNoSuccessAstNode();
			}

			if (Match(TokenType::KW_GOTO))
			{
				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				Consume(TokenType::SEMICOLON);
				return GenerateGotoNode(identifier);
			}

			return GenerateNoSuccessAstNode();
		}

		// Declarations
		static AstNode* ParseDeclarationStatement()
		{
			return ParseBlockDeclaration();
		}

		static AstNode* ParseDeclarationSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseDeclaration();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclaration = ParseDeclarationSequence();
			result = GenerateDeclarationSeqNode(result, nextDeclaration);

			return result;
		}

		static AstNode* ParseUSystemDeclaration()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::USYSTEM))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause();
				if (parameterDeclarationClause->success)
				{
					Consume(TokenType::RIGHT_PAREN);
					Match(TokenType::SEMICOLON);
					AstNode* namedNamespaceDefinition = ParseNamedNamespaceDefinition();
					if (namedNamespaceDefinition->success)
					{
						return GenerateUSystemDeclarationNode(parameterDeclarationClause, namedNamespaceDefinition);
					}
					FreeNode(namedNamespaceDefinition);
				}
				FreeNode(parameterDeclarationClause);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclaration()
		{
			int backtrackPosition = CurrentToken;
			AstNode* blockDeclaration = ParseBlockDeclaration();
			if (blockDeclaration->success)
			{
				return blockDeclaration;
			}
			FreeNode(blockDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* functionDefinition = ParseFunctionDefinition();
			if (functionDefinition->success)
			{
				return functionDefinition;
			}
			FreeNode(functionDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* templateDeclaration = ParseTemplateDeclaration();
			if (templateDeclaration->success)
			{
				return templateDeclaration;
			}
			FreeNode(templateDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* explicitInstantiation = ParseExplicitInstantiation();
			if (explicitInstantiation->success)
			{
				return explicitInstantiation;
			}
			FreeNode(explicitInstantiation);
			BacktrackTo(backtrackPosition);

			AstNode* explicitSpecialization = ParseExplicitSpecialization();
			if (explicitSpecialization->success)
			{
				return explicitSpecialization;
			}
			FreeNode(explicitSpecialization);
			BacktrackTo(backtrackPosition);

			AstNode* linkageSpecification = ParseLinkageSpecification();
			if (linkageSpecification->success)
			{
				return linkageSpecification;
			}
			FreeNode(linkageSpecification);
			BacktrackTo(backtrackPosition);

			AstNode* namespaceDefinition = ParseNamespaceDefinition();
			if (namespaceDefinition->success)
			{
				return namespaceDefinition;
			}
			FreeNode(namespaceDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* emptyDeclaration = ParseEmptyDeclaration();
			if (emptyDeclaration->success)
			{
				return emptyDeclaration;
			}
			FreeNode(emptyDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* attributeDeclaration = ParseAttributeDeclaration();
			if (attributeDeclaration->success)
			{
				return attributeDeclaration;
			}
			FreeNode(attributeDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* uSystemDeclaration = ParseUSystemDeclaration();
			if (uSystemDeclaration->success)
			{
				return uSystemDeclaration;
			}
			FreeNode(uSystemDeclaration);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBlockDeclaration()
		{
			int backtrackPosition = CurrentToken;
			AstNode* simpleDeclaration = ParseSimpleDeclaration();
			if (simpleDeclaration->success)
			{
				return simpleDeclaration;
			}
			FreeNode(simpleDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* asmDefinition = ParseAsmDefinition();
			if (asmDefinition->success)
			{
				return asmDefinition;
			}
			FreeNode(asmDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* namespaceAliasDefinition = ParseNamespaceAliasDefinition();
			if (namespaceAliasDefinition->success)
			{
				return namespaceAliasDefinition;
			}
			FreeNode(namespaceAliasDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* usingDeclaration = ParseUsingDeclaration();
			if (usingDeclaration->success)
			{
				return usingDeclaration;
			}
			FreeNode(usingDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* usingDirective = ParseUsingDirective();
			if (usingDirective->success)
			{
				return usingDirective;
			}
			FreeNode(usingDirective);
			BacktrackTo(backtrackPosition);

			AstNode* staticAssertDeclaration = ParseStaticAssertDeclaration();
			if (staticAssertDeclaration->success)
			{
				return staticAssertDeclaration;
			}
			FreeNode(staticAssertDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* aliasDeclaration = ParseAliasDeclaration();
			if (aliasDeclaration->success)
			{
				return aliasDeclaration;
			}
			FreeNode(aliasDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* opaqueEnumDeclaration = ParseOpaqueEnumDeclaration();
			if (opaqueEnumDeclaration->success)
			{
				return opaqueEnumDeclaration;
			}
			FreeNode(opaqueEnumDeclaration);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAliasDeclaration()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_USING))
			{
				if (Peek() == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
					Consume(TokenType::EQUAL);
					AstNode* typeId = ParseTypeId();
					if (typeId->success)
					{
						Consume(TokenType::SEMICOLON);
						return GenerateAliasDeclarationNode(identifier, typeId);
					}
					FreeNode(typeId);
				}
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseSimpleDeclaration()
		{
			int backtrackPosition = CurrentToken;
			// All optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence();
			AstNode* initDeclaratorList = ParseInitDeclaratorList();
			if (!(attributeSpecifierSeq->success || declSpecifierSeq->success || initDeclaratorList->success))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(initDeclaratorList);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(TokenType::SEMICOLON))
			{
				return GenerateSimpleDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, initDeclaratorList);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			FreeNode(initDeclaratorList);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseStaticAssertDeclaration()
		{
			if (Match(TokenType::KW_STATIC_ASSERT))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* constantExpression = ParseConstantExpression();
				Consume(TokenType::COMMA);
				Token stringLiteral = ConsumeCurrent(TokenType::STRING_LITERAL);
				Consume(TokenType::RIGHT_PAREN);
				Consume(TokenType::SEMICOLON);

				return GenerateStaticAssertDeclarationNode(constantExpression, stringLiteral);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEmptyDeclaration()
		{
			if (Match(TokenType::SEMICOLON))
			{
				return GenerateEmptyStatementNode();
			}
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeDeclaration()
		{
			int backtrackPosition = CurrentToken;
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			if (!attributeSpecifierSeq->success)
			{
				FreeNode(attributeSpecifierSeq);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}
			Consume(TokenType::SEMICOLON);

			return attributeSpecifierSeq;
		}

		static AstNode* ParseDeclarationSpecifier()
		{
			if (Peek() == TokenType::KW_FRIEND || Peek() == TokenType::KW_TYPEDEF || Peek() == TokenType::KW_CONST_EXPR)
			{
				return GenerateSimpleDeclSpecifierNode(ConsumeCurrent(Peek()));
			}

			int backtrackPosition = CurrentToken;
			AstNode* storageClassSpecifier = ParseStorageClassSpecifier();
			if (storageClassSpecifier->success)
			{
				return GenerateDeclSpecifierNode(storageClassSpecifier);
			}
			FreeNode(storageClassSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* typeSpecifier = ParseTypeSpecifier();
			if (typeSpecifier->success)
			{
				return GenerateDeclSpecifierNode(typeSpecifier);
			}
			FreeNode(typeSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* functionSpecifier = ParseFunctionSpecifier();
			if (functionSpecifier->success)
			{
				return GenerateDeclSpecifierNode(functionSpecifier);
			}
			FreeNode(functionSpecifier);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclarationSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseDeclarationSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclSpec = ParseDeclarationSpecifierSequence();
			result = GenerateDeclSpecSeqNode(result, nextDeclSpec, GenerateNoSuccessAstNode());
			if (!nextDeclSpec->success)
			{
				FreeNode(result->declSpecSeq.attributeSpecifierSeq);
				// Optional
				result->declSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			}

			return result;
		}

		static AstNode* ParseStorageClassSpecifier()
		{
			if (PeekIn({ TokenType::KW_AUTO, TokenType::KW_REGISTER, TokenType::KW_STATIC, TokenType::KW_THREAD_LOCAL, TokenType::KW_EXTERN, TokenType::KW_MUTABLE }))
			{
				return GenerateStorageClassSpecNode(ConsumeCurrent(Peek()));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionSpecifier()
		{
			if (PeekIn({ TokenType::KW_INLINE, TokenType::KW_VIRTUAL, TokenType::KW_EXPLICIT }))
			{
				return GenerateFunctionSpecNode(ConsumeCurrent(Peek()));
			}

			return GenerateNoSuccessAstNode();
		}

		// Types/typedefs
		static AstNode* ParseTypedefName()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateTypedefNameNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeSpecifier()
		{
			int backtrackPosition = CurrentToken;
			// TODO: I'm pretty sure a trailing type specifier needs a semicolon before the next '{'
			// TODO: Make this a bit smarter, it can also mess up if we hit a '}' before the next '{'
			if (!LookAheadBeforeSemicolon({ TokenType::LEFT_CURLY_BRACKET }))
			{
				AstNode* trailingTypeSpecifier = ParseTrailingTypeSpecifier();
				if (trailingTypeSpecifier->success)
				{
					return trailingTypeSpecifier;
				}
				FreeNode(trailingTypeSpecifier);
				BacktrackTo(backtrackPosition);
			}

			AstNode* classSpecifier = ParseClassSpecifier();
			if (classSpecifier->success)
			{
				return classSpecifier;
			}
			FreeNode(classSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* enumSpecifier = ParseEnumSpecifier();
			if (enumSpecifier->success)
			{
				return enumSpecifier;
			}
			FreeNode(enumSpecifier);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTrailingTypeSpecifier()
		{
			int backtrackPosition = CurrentToken;
			AstNode* simpleTypeSpecifier = ParseSimpleTypeSpecifier();
			if (simpleTypeSpecifier->success)
			{
				return simpleTypeSpecifier;
			}
			FreeNode(simpleTypeSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* elaboratedTypeSpecifier = ParseElaboratedTypeSpecifier();
			if (elaboratedTypeSpecifier->success)
			{
				return elaboratedTypeSpecifier;
			}
			FreeNode(elaboratedTypeSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* typenameSpecifier = ParseTypenameSpecifier();
			if (typenameSpecifier->success)
			{
				return typenameSpecifier;
			}
			FreeNode(typenameSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* cvQualifier = ParseCvQualifier();
			if (cvQualifier->success)
			{
				return cvQualifier;
			}
			FreeNode(cvQualifier);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseTypeSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextTypeSpecifier = ParseTypeSpecifierSequence();
			result = GenerateTypeSpecSeqNode(result, nextTypeSpecifier, GenerateNoSuccessAstNode());
			if (!nextTypeSpecifier->success)
			{
				FreeNode(result->typeSpecSeq.attributeSpecifierSeq);
				result->typeSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			}

			return result;
		}

		static AstNode* ParseTrailingTypeSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseTrailingTypeSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextTypeSpecifier = ParseTrailingTypeSpecifierSequence();
			result = GenerateTrailingTypeSpecSeqNode(result, nextTypeSpecifier, GenerateNoSuccessAstNode());
			if (!nextTypeSpecifier->success)
			{
				FreeNode(result->trailingTypeSpecSeq.attributeSpecifierSeq);
				result->trailingTypeSpecSeq.attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			}

			return result;
		}

		static AstNode* ParseSimpleTypeSpecifier()
		{
			if (PeekIn({ TokenType::KW_CHAR, TokenType::KW_CHAR16_T, TokenType::KW_CHAR32_T, TokenType::KW_WCHAR_T, TokenType::KW_BOOL, TokenType::KW_SHORT, TokenType::KW_INT,
				TokenType::KW_LONG, TokenType::KW_SIGNED, TokenType::KW_UNSIGNED, TokenType::KW_FLOAT, TokenType::KW_DOUBLE, TokenType::KW_VOID, TokenType::KW_AUTO }))
			{
				return GenerateSimpleTypeTokenSpecNode(ConsumeCurrent(Peek()));
			}

			int backtrackPosition = CurrentToken;
			AstNode* decltypeSpecifier = ParseDecltypeSpecifier();
			if (decltypeSpecifier->success)
			{
				return decltypeSpecifier;
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
			}

			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			if (Match(TokenType::KW_TEMPLATE))
			{
				if (!nestedNameSpecifier->success)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(backtrackPosition);
				}
				AstNode* simpleTemplateId = ParseSimpleTemplateId();
				if (simpleTemplateId->success)
				{
					return GenerateSimpleTypeTemplateSpecNode(nestedNameSpecifier, simpleTemplateId);
				}
				FreeNode(simpleTemplateId);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* typeName = ParseTypeName();
			if (typeName->success)
			{
				return GenerateSimpleTypeSpecNode(nestedNameSpecifier, typeName);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(typeName);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeName()
		{
			int backtrackPosition = CurrentToken;
			AstNode* simpleTemplateId = ParseSimpleTemplateId();
			if (simpleTemplateId->success)
			{
				return simpleTemplateId;
			}
			FreeNode(simpleTemplateId);
			BacktrackTo(backtrackPosition);

			AstNode* className = ParseClassName();
			if (className->success)
			{
				return className;
			}
			FreeNode(className);
			BacktrackTo(backtrackPosition);

			AstNode* enumName = ParseEnumName();
			if (enumName->success)
			{
				return enumName;
			}
			FreeNode(enumName);
			BacktrackTo(backtrackPosition);

			AstNode* typedefName = ParseTypedefName();
			if (typedefName->success)
			{
				return typedefName;
			}
			FreeNode(typedefName);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDecltypeSpecifier()
		{
			if (Match(TokenType::KW_DECLTYPE))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* expression = ParseExpression();
				Consume(TokenType::RIGHT_PAREN);
				return GenerateDecltypeSpecNode(expression);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseElaboratedTypeSpecifier()
		{
			if (Peek() == TokenType::KW_ENUM)
			{
				bool hasScopeOp = Match(TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(TokenType::COLON);
				}

				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				return GenerateElaboratedSpecifierEnumNode(nestedNameSpecifier, identifier, hasScopeOp);
			}

			int backtrackPosition = CurrentToken;
			AstNode* classKey = ParseClassKey();
			if (classKey->success)
			{
				int backtrackPosition2 = CurrentToken;
				bool hasScopeOp = Match(TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(TokenType::COLON);
				}

				// TODO: Test if this actually works right...
				bool isTemplate = LookAheadBeforeSemicolon({ TokenType::LEFT_ANGLE_BRACKET });
				if (isTemplate)
				{
					// Optional
					AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
					if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
					{
						FreeNode(nestedNameSpecifier);
						nestedNameSpecifier = ParseNestedNameSpecifier();
					}
					bool hasTemplateKeyword = Match(TokenType::KW_TEMPLATE);
					AstNode* simpleTemplateId = ParseSimpleTemplateId();
					if (simpleTemplateId->success)
					{
						return GenerateElaboratedSpecifierTemplateNode(classKey, nestedNameSpecifier, simpleTemplateId, hasScopeOp, hasTemplateKeyword);
					}
					FreeNode(simpleTemplateId);
					FreeNode(nestedNameSpecifier);
					BacktrackTo(backtrackPosition2);
				}

				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				hasScopeOp = Match(TokenType::COLON);
				if (hasScopeOp)
				{
					Consume(TokenType::COLON);
				}

				// Optional 
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				if (Peek() == TokenType::IDENTIFIER)
				{
					Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
					return GenerateElaboratedSpecifierClassNode(classKey, attributeSpecifierSeq, nestedNameSpecifier, identifier, hasScopeOp);
				}
				FreeNode(nestedNameSpecifier);
				FreeNode(attributeSpecifierSeq);
			}

			FreeNode(classKey);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Enums
		static AstNode* ParseEnumName()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateEnumNameNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumSpecifier()
		{
			int backtrackPosition = CurrentToken;
			AstNode* enumHead = ParseEnumHead();
			if (enumHead->success)
			{
				Consume(TokenType::LEFT_BRACKET);
				// This is optional it's ok if it fails
				AstNode* enumeratorList = ParseEnumeratorList();
				if (enumeratorList->success)
				{
					// We don't really care about this, but we want to make sure to parse it if it's there
					bool trailingComma = Match(TokenType::COMMA);
				}
				Consume(TokenType::RIGHT_BRACKET);

				return GenerateEnumSpecifierNode(enumHead, enumeratorList);
			}

			FreeNode(enumHead);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumHead()
		{
			int backtrackPosition = CurrentToken;
			AstNode* enumKey = ParseEnumKey();
			if (!enumKey->success)
			{
				FreeNode(enumKey);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// This is optional
			AstNode* attributeSpecifierSequence = ParseAttributeSpecifierSequence();

			backtrackPosition = CurrentToken;
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			if (nestedNameSpecifier->success)
			{
				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				// This is also optional
				AstNode* enumBase = ParseEnumBase();

				return GenerateEnumHeadNode(enumKey, attributeSpecifierSequence, nestedNameSpecifier, identifier, enumBase);
			}

			FreeNode(nestedNameSpecifier);
			BacktrackTo(backtrackPosition);

			Token identifier;
			identifier.m_Type = TokenType::None;
			if (Peek() == TokenType::IDENTIFIER)
			{
				identifier = ConsumeCurrent(TokenType::IDENTIFIER);
			}

			// enum base is optional so this is fine
			return GenerateEnumHeadNode(enumKey, attributeSpecifierSequence, GenerateNoSuccessAstNode(), identifier, ParseEnumBase());
		}

		static AstNode* ParseOpaqueEnumDeclaration()
		{
			int backtrackPosition = CurrentToken;
			AstNode* enumKey = ParseEnumKey();
			if (!enumKey->success)
			{
				BacktrackTo(backtrackPosition);
				FreeNode(enumKey);
				return GenerateNoSuccessAstNode();
			}

			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
			AstNode* enumBase = ParseEnumBase();
			Consume(TokenType::SEMICOLON);

			return GenerateOpaqueEnumDeclNode(enumKey, attributeSpecifierSeq, identifier, enumBase);
		}

		static AstNode* ParseEnumKey()
		{
			if (Match(TokenType::KW_ENUM))
			{
				if (Match(TokenType::KW_CLASS))
				{
					return GenerateEnumKeyNode(EnumKeyType::Class);
				}

				if (Match(TokenType::KW_STRUCT))
				{
					return GenerateEnumKeyNode(EnumKeyType::Struct);
				}

				return GenerateEnumKeyNode(EnumKeyType::Enum);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumBase()
		{
			if (Match(TokenType::COLON))
			{
				AstNode* typeSpecifierSequence = ParseTypeSpecifierSequence();
				return GenerateEnumBaseNode(typeSpecifierSequence);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseEnumeratorList()
		{
			AstNode* result = ParseEnumeratorDefinition();

			while (Match(TokenType::COMMA))
			{
				result = GenerateEnumeratorListNode(result, ParseEnumeratorList());
			}

			return GenerateEnumeratorListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseEnumeratorDefinition()
		{
			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);

			AstNode* constantExpression = Match(TokenType::EQUAL) ?
				ParseConstantExpression() :
				GenerateNoSuccessAstNode();

			return GenerateEnumeratorDefinitionNode(identifier, constantExpression);
		}

		// Namespaces
		static AstNode* ParseNamespaceName()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateNamespaceNameNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamespaceDefinition()
		{
			int backtrackPosition = CurrentToken;
			AstNode* namedNamespaceDefinition = ParseNamedNamespaceDefinition();
			if (namedNamespaceDefinition->success)
			{
				return namedNamespaceDefinition;
			}
			FreeNode(namedNamespaceDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* unnamedNamespaceDefinition = ParseUnnamedNamespaceDefinition();
			if (unnamedNamespaceDefinition->success)
			{
				return unnamedNamespaceDefinition;
			}
			FreeNode(unnamedNamespaceDefinition);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamedNamespaceDefinition()
		{
			int backtrackPosition = CurrentToken;
			bool isInline = Match(TokenType::KW_INLINE);
			if (Match(TokenType::KW_NAMESPACE))
			{
				if (!(Peek() == TokenType::IDENTIFIER))
				{
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				Consume(TokenType::LEFT_CURLY_BRACKET);
				AstNode* namespaceBody = ParseNamespaceBody();
				Consume(TokenType::RIGHT_CURLY_BRACKET);

				return GenerateNamedNamespaceDefinitionNode(isInline, identifier, namespaceBody);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUnnamedNamespaceDefinition()
		{
			int backtrackPosition = CurrentToken;
			bool isInline = Match(TokenType::KW_INLINE);
			if (Match(TokenType::KW_NAMESPACE))
			{
				if (Peek() != TokenType::LEFT_BRACKET)
				{
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Consume(TokenType::LEFT_BRACKET);
				AstNode* namespaceBody = ParseNamespaceBody();
				Consume(TokenType::RIGHT_BRACKET);

				return GenerateUnnamedNamespaceDefinitionNode(isInline, namespaceBody);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNamespaceBody()
		{
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* declarationSequence = ParseDeclarationSequence();
			if (declarationSequence->success)
			{
				return declarationSequence;
			}

			FreeNode(declarationSequence);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Namespace alias
		static AstNode* ParseNamespaceAliasDefinition()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_NAMESPACE))
			{
				Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				if (!Match(TokenType::EQUAL))
				{
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				AstNode* qualifiedNamespaceSpecifier = ParseQualifiedNamespaceSpecifier();
				Consume(TokenType::SEMICOLON);

				return GenerateNamespaceAliasDefinitionNode(identifier, qualifiedNamespaceSpecifier);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseQualifiedNamespaceSpecifier()
		{
			int backtrackPosition = CurrentToken;

			bool isNested = Match(TokenType::COLON);
			if (isNested) Consume(TokenType::COLON);

			// This is optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			AstNode* namespaceName = ParseNamespaceName();
			if (namespaceName->success)
			{
				return GenerateQualifiedNamespaceSpecifierNode(isNested, nestedNameSpecifier, namespaceName);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(namespaceName);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Using
		static AstNode* ParseUsingDeclaration()
		{
			if (Match(TokenType::KW_USING))
			{
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
					AstNode* unqualifiedId = ParseUnqualifiedId();
					Consume(TokenType::SEMICOLON);
					return GenerateUsingDeclarationNode(unqualifiedId);
				}

				bool isTypename = Match(TokenType::KW_TYPENAME);
				bool isNested = Match(TokenType::COLON);
				if (isNested) Consume(TokenType::COLON);
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				AstNode* unqualifiedId = ParseUnqualifiedId();
				Consume(TokenType::SEMICOLON);

				return GenerateUsingTypenameDeclarationNode(isTypename, isNested, nestedNameSpecifier, unqualifiedId);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseUsingDirective()
		{
			int backtrackPosition = CurrentToken;

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			if (Match(TokenType::KW_USING))
			{
				bool isNested = Match(TokenType::COLON);
				if (isNested) Match(TokenType::COLON);

				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				AstNode* namespaceName = ParseNamespaceName();
				Consume(TokenType::SEMICOLON);

				return GenerateUsingDirectiveNode(attributeSpecifierSeq, isNested, nestedNameSpecifier, namespaceName);
			}

			FreeNode(attributeSpecifierSeq);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAsmDefinition()
		{
			if (Match(TokenType::KW_ASM))
			{
				Consume(TokenType::LEFT_PAREN);
				Token stringLiteral = ConsumeCurrent(TokenType::STRING_LITERAL);
				Consume(TokenType::RIGHT_PAREN);
				Consume(TokenType::SEMICOLON);

				return GenerateAsmNode(stringLiteral);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseLinkageSpecification()
		{
			if (Match(TokenType::KW_EXTERN))
			{
				Token stringLiteral = ConsumeCurrent(TokenType::STRING_LITERAL);
				if (Match(TokenType::LEFT_BRACKET))
				{
					// Optional
					AstNode* declarationSeq = ParseDeclarationSequence();
					Consume(TokenType::RIGHT_BRACKET);

					return GenerateLinkageSpecificationBlockNode(stringLiteral, declarationSeq);
				}
				else
				{
					AstNode* declaration = ParseDeclaration();
					return GenerateLinkageSpecificationNode(stringLiteral, declaration);
				}
			}

			return GenerateNoSuccessAstNode();
		}

		// Attribute Specifiers
		static AstNode* ParseAttributeSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseAttributeSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextSpec = ParseAttributeSpecifier();
			result = GenerateAttributeSpecifierSequenceNode(result, nextSpec);

			return result;
		}

		static AstNode* ParseAttributeSpecifier()
		{
			if (Match(TokenType::LEFT_BRACKET))
			{
				Consume(TokenType::LEFT_BRACKET);
				AstNode* node = ParseAttributeList();
				Consume(TokenType::RIGHT_BRACKET);
				Consume(TokenType::RIGHT_BRACKET);
				return node;
			}

			return ParseAlignmentSpecifier();
		}

		static AstNode* ParseAlignmentSpecifier()
		{
			if (Match(TokenType::KW_ALIGN_AS))
			{
				Consume(TokenType::LEFT_PAREN);
				int backtrackPosition = CurrentToken;
				AstNode* typeId = ParseTypeId();
				if (typeId->success)
				{
					bool hasElipsis = Match(TokenType::DOT);
					if (hasElipsis)
					{
						Consume(TokenType::DOT); Consume(TokenType::DOT);
					}
					Consume(TokenType::RIGHT_PAREN);

					return GenerateAlignAsTypeIdNode(typeId, hasElipsis);
				}
				FreeNode(typeId);
				BacktrackTo(backtrackPosition);

				AstNode* alignmentExpression = ParseAlignmentExpression();
				bool hasElipsis = Match(TokenType::DOT);
				if (hasElipsis)
				{
					Consume(TokenType::DOT); Consume(TokenType::DOT);
				}
				Consume(TokenType::RIGHT_PAREN);

				return GenerateAlignAsExpressionNode(alignmentExpression, hasElipsis);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseAttribute();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);

				return GenerateEmptyAttributeListNode();
			}

			while (Match(TokenType::COMMA))
			{
				result = GenerateAttributeListNode(result, ParseAttributeList());
			}

			bool trailingComma = Match(TokenType::COMMA);
			bool elipsis = false;
			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT); Consume(TokenType::DOT);
				elipsis = true;
				if (trailingComma)
				{
					ErrorAtToken();
				}
			}

			return result;
		}

		static AstNode* ParseAttribute()
		{
			int backtrackPosition = CurrentToken;
			AstNode* attributeToken = ParseAttributeToken();
			if (!attributeToken->success)
			{
				FreeNode(attributeToken);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}
			// Optional
			AstNode* attributeArgumentClause = ParseAttributeArgumentClause();
			return GenerateAttributeNode(attributeToken, attributeArgumentClause);
		}

		static AstNode* ParseAttributeToken()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				Token id1 = ConsumeCurrent(TokenType::IDENTIFIER);
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
					Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
					return GenerateAttributeTokenNode(id1, identifier);
				}

				Token empty;
				empty.m_Type = TokenType::None;
				return GenerateAttributeTokenNode(empty, id1);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseAttributeArgumentClause()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* balancedTokenSeq = ParseBalancedTokenSequence();
				Consume(TokenType::RIGHT_PAREN);

				return GenerateAttributeArgumentClauseNode(balancedTokenSeq);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBalancedTokenSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseBalancedToken();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextBalancedToken = ParseBalancedTokenSequence();
			result = GenerateBalancedTokenSeqNode(result, nextBalancedToken);

			return result;
		}

		static AstNode* ParseBalancedToken()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* result = ParseBalancedTokenSequence();
				Consume(TokenType::RIGHT_PAREN);
				return result;
			}

			if (Match(TokenType::LEFT_BRACKET))
			{
				AstNode* result = ParseBalancedTokenSequence();
				Consume(TokenType::RIGHT_BRACKET);
				return result;
			}

			if (Match(TokenType::LEFT_CURLY_BRACKET))
			{
				AstNode* result = ParseBalancedTokenSequence();
				Consume(TokenType::RIGHT_CURLY_BRACKET);
				return result;
			}

			return GenerateBalancedTokenNode(ConsumeCurrent(Peek()));
		}

		// Declarations
		static AstNode* ParseInitDeclaratorList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseInitDeclarator();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextDeclarator = ParseInitDeclaratorList();
			result = GenerateInitDeclaratorListNode(result, nextDeclarator);

			return result;
		}

		static AstNode* ParseInitDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* declarator = ParseDeclarator();
			if (!declarator->success)
			{
				FreeNode(declarator);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* initializer = ParseInitializer();

			return GenerateInitDeclaratorNode(declarator, initializer);
		}

		static AstNode* ParseDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* noPtrDeclarator = ParseNoPtrDeclarator();
			if (noPtrDeclarator->success)
			{
				AstNode* parametersAndQualifiers = ParseParametersAndQualifiers();
				AstNode* trailingReturnType = ParseTrailingReturnType();
				if (parametersAndQualifiers->success && trailingReturnType->success)
				{
					return GenerateDeclaratorNode(noPtrDeclarator, parametersAndQualifiers, trailingReturnType);
				}
				FreeNode(parametersAndQualifiers);
				FreeNode(trailingReturnType);
			}
			FreeNode(noPtrDeclarator);
			BacktrackTo(backtrackPosition);

			AstNode* ptrDeclarator = ParsePtrDeclarator();
			if (ptrDeclarator->success)
			{
				return ptrDeclarator;
			}

			FreeNode(ptrDeclarator);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* noPtrDeclarator = ParseNoPtrDeclarator();
			if (noPtrDeclarator->success)
			{
				return noPtrDeclarator;
			}
			FreeNode(noPtrDeclarator);
			BacktrackTo(backtrackPosition);

			AstNode* ptrOperator = ParsePtrOperator();
			if (ptrOperator->success)
			{
				AstNode* ptrDeclarator = ParsePtrDeclarator();
				if (ptrDeclarator->success)
				{
					return GeneratePtrDeclaratorNode(ptrOperator, ptrDeclarator);
				}
				FreeNode(ptrDeclarator);
			}

			FreeNode(ptrOperator);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseNoPtrDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers();
			if (parametersAndQualifiers->success)
			{
				AstNode* noptrDeclarator = ParseNoPtrDeclarator();
				return GenerateNoPtrParamAndQualDeclaratorNode(parametersAndQualifiers, noptrDeclarator);
			}
			FreeNode(parametersAndQualifiers);
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* ptrDeclarator = ParsePtrDeclarator();
				if (ptrDeclarator->success)
				{
					return GenerateNoPtrParenDeclaratorNode(ptrDeclarator);
				}
				FreeNode(ptrDeclarator);
				BacktrackTo(backtrackPosition);
			}

			if (Match(TokenType::LEFT_BRACKET))
			{
				AstNode* constantExpression = ParseConstantExpression();
				Consume(TokenType::RIGHT_BRACKET);
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				AstNode* noptrDeclarator = ParseNoPtrDeclarator();
				return GenerateNoPtrBracketDeclaratorNode(constantExpression, attributeSpecifierSeq, noptrDeclarator);
			}

			AstNode* declaratorId = ParseDeclaratorId();
			if (declaratorId->success)
			{
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				return GenerateNoPtrDeclaratorNode(declaratorId, attributeSpecifierSeq);
			}
			FreeNode(declaratorId);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseParametersAndQualifiers()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* parameterDeclarationClause = ParseParameterDeclarationClause();
				Consume(TokenType::RIGHT_PAREN);
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				// Optional
				AstNode* cvQualifierSeq = ParseCvQualifierSequence();
				// Optional 
				AstNode* refQualifier = ParseRefQualifier();
				// Optional
				AstNode* exceptionSpec = ParseExceptionSpecification();

				return GenerateParametersAndQualifiersNode(parameterDeclarationClause, attributeSpecifierSeq, cvQualifierSeq, refQualifier, exceptionSpec);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTrailingReturnType()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::ARROW))
			{
				AstNode* trailingTypeSpecifierSeq = ParseTrailingTypeSpecifierSequence();
				if (!trailingTypeSpecifierSeq->success)
				{
					FreeNode(trailingTypeSpecifierSeq);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				// Optional
				AstNode* abstractDeclarator = ParseAbstractDeclarator();

				return GenerateTrailingReturnTypeNode(trailingTypeSpecifierSeq, abstractDeclarator);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrOperator()
		{
			if (Match(TokenType::STAR))
			{
				// Optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				// Optional
				AstNode* cvQualifierSeq = ParseCvQualifierSequence();
				return GeneratePtrStarNode(attributeSpecifierSeq, cvQualifierSeq);
			}

			if (Match(TokenType::AND))
			{
				if (Match(TokenType::AND))
				{
					return GenerateRefRefNode(ParseAttributeSpecifierSequence());
				}

				return GenerateRefNode(ParseAttributeSpecifierSequence());
			}

			int backtrackPosition = CurrentToken;
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				if (!nestedNameSpecifier)
				{
					FreeNode(nestedNameSpecifier);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				Consume(TokenType::STAR);
				// Both optional
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				AstNode* cvQualifierSeq = ParseCvQualifierSequence();
				return GeneratePtrNamespaceStarNode(nestedNameSpecifier, attributeSpecifierSeq, cvQualifierSeq);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseCvQualifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseCvQualifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextQualifier = ParseCvQualifierSequence();
				result = GenerateCvQualifierSeqNode(result, nextQualifier);
				if (!nextQualifier->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseCvQualifier()
		{
			if (Peek() == TokenType::KW_CONST || Peek() == TokenType::KW_VOLATILE)
			{
				Token token = ConsumeCurrent(Peek());
				return GenerateCvQualifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseRefQualifier()
		{
			if (Match(TokenType::AND))
			{
				bool doubleRef = Match(TokenType::AND);
				return GenerateRefQualifierNode(doubleRef);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDeclaratorId()
		{
			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
				return ParseIdExpression();
			}

			int backtrackPosition = CurrentToken;
			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
				// Optional
				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				AstNode* className = ParseClassName();
				if (!className->success)
				{
					FreeNode(nestedNameSpecifier);
					FreeNode(className);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateDeclaratorIdNode(nestedNameSpecifier, className);
			}

			backtrackPosition = CurrentToken;
			AstNode* idExpression = ParseIdExpression();
			if (idExpression->success)
			{
				return idExpression;
			}
			FreeNode(idExpression);
			BacktrackTo(backtrackPosition);

			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			AstNode* className = ParseClassName();
			if (className->success)
			{
				return GenerateDeclaratorIdNode(nestedNameSpecifier, className);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(className);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// dcl.name
		static AstNode* ParseTypeId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* typeSpecifierSequence = ParseTypeSpecifierSequence();
			if (!typeSpecifierSequence->success)
			{
				FreeNode(typeSpecifierSequence);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* abstractDeclarator = ParseAbstractDeclarator();
			return GenerateTypeIdNode(typeSpecifierSequence, abstractDeclarator);
		}

		static AstNode* ParseAbstractDeclarator()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::DOT))
			{
				if (Match(TokenType::DOT))
				{
					if (Match(TokenType::DOT))
					{
						return GenerateAbstractElipsisDeclaratorNode();
					}
				}
			}
			BacktrackTo(backtrackPosition);

			// Optional
			AstNode* noptrAbstractDeclarator = ParseNoptrAbstractDeclarator();
			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers();
			if (parametersAndQualifiers->success)
			{
				AstNode* trailingReturnType = ParseTrailingReturnType();
				if (trailingReturnType->success)
				{
					return GenerateAbstractDeclaratorNode(noptrAbstractDeclarator, parametersAndQualifiers, trailingReturnType);
				}
				FreeNode(trailingReturnType);
			}
			FreeNode(noptrAbstractDeclarator);
			FreeNode(parametersAndQualifiers);
			BacktrackTo(backtrackPosition);

			AstNode* ptrAbstractDeclarator = ParsePtrAbstractDeclarator();
			if (ptrAbstractDeclarator->success)
			{
				return ptrAbstractDeclarator;
			}

			FreeNode(ptrAbstractDeclarator);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePtrAbstractDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* ptrOperator = ParsePtrOperator();
			if (ptrOperator->success)
			{
				// Optional
				AstNode* ptrAbstractDeclarator = ParsePtrAbstractDeclarator();
				return GeneratePtrAbstractDeclaratorNode(ptrOperator, ptrAbstractDeclarator);
			}
			FreeNode(ptrOperator);
			BacktrackTo(backtrackPosition);

			return ParseNoptrAbstractDeclarator();
		}

		static AstNode* ParseNoptrAbstractDeclarator()
		{
			// TODO: Not sure if this will work right...?
			int backtrackPosition = CurrentToken;
			AstNode* ptrAbstractDeclarator = GenerateNoSuccessAstNode();
			if (Match(TokenType::LEFT_PAREN))
			{
				ptrAbstractDeclarator = ParsePtrAbstractDeclarator();
				if (!ptrAbstractDeclarator->success)
				{
					FreeNode(ptrAbstractDeclarator);
					BacktrackTo(backtrackPosition);
				}
				Consume(TokenType::RIGHT_PAREN);
			}

			if (Match(TokenType::LEFT_BRACKET))
			{
				AstNode* constantExpression = ParseConstantExpression();
				if (!constantExpression->success)
				{
					FreeNode(constantExpression);
					FreeNode(ptrAbstractDeclarator);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}
				AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
				return GenerateNoptrAbstractExpressionDeclaratorNode(ptrAbstractDeclarator, constantExpression, attributeSpecifierSeq, ParseNoptrAbstractDeclarator());
			}

			AstNode* parametersAndQualifiers = ParseParametersAndQualifiers();
			if (parametersAndQualifiers->success)
			{
				return GenerateNoptrAbstractDeclaratorNode(ptrAbstractDeclarator, parametersAndQualifiers, ParseNoptrAbstractDeclarator());
			}

			FreeNode(parametersAndQualifiers);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// dcl.fct
		static AstNode* ParseParameterDeclarationClause()
		{
			// Optional
			AstNode* parameterDeclarationList = ParseParameterDeclarationList();
			if (parameterDeclarationList->success)
			{
				if (Match(TokenType::COMMA))
				{
					if (Match(TokenType::DOT))
					{
						Consume(TokenType::DOT);
						Consume(TokenType::DOT);
					}
				}
				return parameterDeclarationList;
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return parameterDeclarationList;
		}

		static AstNode* ParseParameterDeclarationList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseParameterDeclaration();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(TokenType::COMMA))
			{
				AstNode* nextParameter = ParseParameterDeclarationList();
				result = GenerateParameterDeclarationListNode(result, nextParameter);
			}

			return result;
		}

		static AstNode* ParseParameterDeclaration()
		{
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence();
			if (declSpecifierSeq->success)
			{
				int backtrackPosition2 = CurrentToken;
				AstNode* declarator = ParseDeclarator();
				if (declarator->success)
				{
					if (Match(TokenType::EQUAL))
					{
						AstNode* initializerClause = ParseInitializerClause();
						if (!initializerClause->success)
						{
							FreeNode(initializerClause);
							FreeNode(declarator);
							FreeNode(attributeSpecifierSeq);
							FreeNode(declSpecifierSeq);
							BacktrackTo(backtrackPosition);
							return GenerateNoSuccessAstNode();
						}

						return GenerateParameterDefaultDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, declarator, initializerClause);
					}

					return GenerateParameterDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, declarator);
				}
				FreeNode(declarator);
				BacktrackTo(backtrackPosition2);

				// Optional 
				AstNode* abstractDeclarator = ParseAbstractDeclarator();
				if (Match(TokenType::EQUAL))
				{
					AstNode* initializerClause = ParseInitializerClause();
					if (!initializerClause->success)
					{
						FreeNode(initializerClause);
						FreeNode(abstractDeclarator);
						FreeNode(attributeSpecifierSeq);
						FreeNode(declSpecifierSeq);
						BacktrackTo(backtrackPosition);
						return GenerateNoSuccessAstNode();
					}

					return GenerateParameterAbstractDefaultDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, abstractDeclarator, initializerClause);
				}

				return GenerateParameterAbstractDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, abstractDeclarator);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Functions
		static AstNode* ParseFunctionDefinition()
		{
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence();
			AstNode* declarator = ParseDeclarator();
			if (!declarator->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(TokenType::EQUAL))
			{
				if (Match(TokenType::KW_DEFAULT))
				{
					Consume(TokenType::SEMICOLON);
					return GenerateFunctionDefaultDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, AutoFunctionType::Default);
				}

				if (Match(TokenType::KW_DELETE))
				{
					Consume(TokenType::SEMICOLON);
					return GenerateFunctionDefaultDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, AutoFunctionType::Delete);
				}

				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(declarator);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* functionBody = ParseFunctionBody();
			if (functionBody->success)
			{
				return GenerateFunctionDefinitionNode(attributeSpecifierSeq, declSpecifierSeq, declarator, functionBody);
			}

			FreeNode(functionBody);
			FreeNode(attributeSpecifierSeq);
			FreeNode(declSpecifierSeq);
			FreeNode(declarator);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionBody()
		{
			int backtrackPosition = CurrentToken;
			AstNode* functionTryBlock = ParseFunctionTryBlock();
			if (functionTryBlock->success)
			{
				return functionTryBlock;
			}
			FreeNode(functionTryBlock);
			BacktrackTo(backtrackPosition);

			// Optional
			AstNode* ctorInitializer = ParseCtorInitializer();
			AstNode* compoundStatement = ParseCompoundStatement();
			if (compoundStatement->success)
			{
				return GenerateFunctionBodyNode(ctorInitializer, compoundStatement);
			}

			FreeNode(ctorInitializer);
			FreeNode(compoundStatement);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Init
		static AstNode* ParseInitializer()
		{
			if (Match(TokenType::LEFT_PAREN))
			{
				AstNode* expressionList = ParseExpressionList();
				Consume(TokenType::RIGHT_PAREN);
				return expressionList;
			}

			return ParseBraceOrEqualInitializer();
		}

		static AstNode* ParseBraceOrEqualInitializer()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::EQUAL))
			{
				AstNode* result = ParseInitializerClause();
				if (result->success)
				{
					return result;
				}
				FreeNode(result);
				BacktrackTo(backtrackPosition);
			}

			return ParseBracedInitList();
		}

		static AstNode* ParseInitializerClause()
		{
			int backtrackPosition = CurrentToken;
			AstNode* bracedInitList = ParseBracedInitList();
			if (bracedInitList->success)
			{
				return bracedInitList;
			}
			FreeNode(bracedInitList);
			BacktrackTo(backtrackPosition);

			AstNode* assignmentExpression = ParseAssignmentExpression();
			if (assignmentExpression->success)
			{
				return assignmentExpression;
			}
			FreeNode(assignmentExpression);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseInitializerList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseInitializerClause();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* nextInitList = nullptr;
			if (Match(TokenType::COMMA))
			{
				nextInitList = ParseInitializerList();
			}
			else
			{
				nextInitList = GenerateNoSuccessAstNode();
			}

			bool hasElipsis = Match(TokenType::DOT);
			if (hasElipsis)
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return GenerateInitializerListNode(result, nextInitList);
		}

		static AstNode* ParseBracedInitList()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::LEFT_CURLY_BRACKET))
			{
				AstNode* initializerList = ParseInitializerList();
				if (initializerList->success)
				{
					bool trailingComma = Match(TokenType::COMMA);
					if (Match(TokenType::RIGHT_CURLY_BRACKET))
					{
						return GenerateBracedInitListNode(initializerList);
					}
				}
				FreeNode(initializerList);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Classes
		static AstNode* ParseClassName()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateClassNameNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return ParseSimpleTemplateId();
		}

		static AstNode* ParseClassSpecifier()
		{
			int backtrackPosition = CurrentToken;
			AstNode* classHead = ParseClassHead();
			if (!classHead->success)
			{
				FreeNode(classHead);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			Consume(TokenType::LEFT_CURLY_BRACKET);
			// Optional
			AstNode* memberSpecification = ParseMemberSpecification();
			Consume(TokenType::RIGHT_CURLY_BRACKET);

			return GenerateClassSpecifierNode(classHead, memberSpecification);
		}

		static AstNode* ParseClassHead()
		{
			int backtrackPosition = CurrentToken;
			AstNode* classKey = ParseClassKey();
			if (!classKey->success)
			{
				FreeNode(classKey);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();

			backtrackPosition = CurrentToken;
			AstNode* classHeadName = ParseClassHeadName();
			if (classHeadName->success)
			{
				// Optional
				AstNode* classVirtSpecifierSeq = ParseClassVirtSpecifierSequence();
				// Optional
				AstNode* baseClause = ParseBaseClause();
				return GenerateClassVirtualHeadNode(classKey, attributeSpecifierSeq, classHeadName, classVirtSpecifierSeq, baseClause);
			}
			FreeNode(classHeadName);
			BacktrackTo(backtrackPosition);

			// Optional
			AstNode* baseClause = ParseBaseClause();
			return GenerateClassHeadNode(classKey, attributeSpecifierSeq, baseClause);
		}

		static AstNode* ParseClassHeadName()
		{
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				// Make sure there's a chance of a nested name before blindly consuming it
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}

			AstNode* className = ParseClassName();
			if (!className->success)
			{
				FreeNode(nestedNameSpecifier);
				FreeNode(className);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateClassHeadNameNode(nestedNameSpecifier, className);
		}

		static AstNode* ParseClassVirtSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseClassVirtSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextSpec = ParseClassVirtSpecifierSequence();
				result = GenerateClassVirtSpecifierSeqNode(result, nextSpec);
				if (!nextSpec->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseClassVirtSpecifier()
		{
			if (Peek() == TokenType::KW_FINAL || Peek() == TokenType::KW_EXPLICIT)
			{
				Token token = ConsumeCurrent(Peek());
				return GenerateClassVirtSpecifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseClassKey()
		{
			if (Peek() == TokenType::KW_CLASS || Peek() == TokenType::KW_STRUCT || Peek() == TokenType::KW_UNION)
			{
				Token token = ConsumeCurrent(Peek());
				return GenerateClassKeyNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		// Class Members
		static AstNode* ParseMemberSpecification()
		{
			int backtrackPosition = CurrentToken;
			AstNode* accessSpecifier = ParseAccessSpecifier();
			if (accessSpecifier->success)
			{
				Consume(TokenType::COLON);
				// Optional
				AstNode* memberSpecification = ParseMemberSpecification();
				return GenerateMemberAndAccessSpecifierNode(accessSpecifier, memberSpecification);
			}
			FreeNode(accessSpecifier);
			BacktrackTo(backtrackPosition);

			AstNode* memberDeclaration = ParseMemberDeclaration();
			if (memberDeclaration->success)
			{
				// Optional
				AstNode* memberSpecification = ParseMemberSpecification();
				return GenerateMemberSpecifierNode(memberDeclaration, memberSpecification);
			}

			FreeNode(memberDeclaration);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseMemberDeclaration()
		{
			// TODO: Does this really work right...?
			int backtrackPosition = CurrentToken;
			AstNode* functionDefinition = ParseFunctionDefinition();
			if (functionDefinition->success)
			{
				bool trailingSemicolon = Match(TokenType::SEMICOLON);
				return GenerateMemberFunctionDeclarationNode(functionDefinition, trailingSemicolon);
			}
			FreeNode(functionDefinition);
			BacktrackTo(backtrackPosition);

			AstNode* usingDeclaration = ParseUsingDeclaration();
			if (usingDeclaration->success)
			{
				return usingDeclaration;
			}
			FreeNode(usingDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* staticAssertDeclaration = ParseStaticAssertDeclaration();
			if (staticAssertDeclaration->success)
			{
				return staticAssertDeclaration;
			}
			FreeNode(staticAssertDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* templateDeclaration = ParseTemplateDeclaration();
			if (templateDeclaration->success)
			{
				return templateDeclaration;
			}
			FreeNode(templateDeclaration);
			BacktrackTo(backtrackPosition);

			AstNode* aliasDeclaration = ParseAliasDeclaration();
			if (aliasDeclaration->success)
			{
				return aliasDeclaration;
			}
			FreeNode(aliasDeclaration);
			BacktrackTo(backtrackPosition);

			// All optional except semicolon
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* declSpecifierSeq = ParseDeclarationSpecifierSequence();
			AstNode* memberDeclaratorList = ParseMemberDeclaratorList();
			if (!Match(TokenType::SEMICOLON))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(declSpecifierSeq);
				FreeNode(memberDeclaratorList);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemberDeclarationNode(attributeSpecifierSeq, declSpecifierSeq, memberDeclaratorList);
		}

		static AstNode* ParseMemberDeclaratorList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseMemberDeclarator();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextDeclarator = ParseMemberDeclaratorList();
				result = GenerateMemberDeclaratorListNode(result, nextDeclarator);
				if (!nextDeclarator->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseMemberDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* declarator = ParseDeclarator();
			if (declarator->success)
			{
				// Optional
				AstNode* virtSpecifierSeq = ParseVirtSpecifierSequence();
				// Also optional, but there's a chance we could be referring to a different node
				int backtrackPosition2 = CurrentToken;
				AstNode* pureSpecifier = ParsePureSpecifier();
				if (pureSpecifier->success)
				{
					return GenerateMemberDeclaratorPureNode(declarator, virtSpecifierSeq, pureSpecifier);
				}
				FreeNode(pureSpecifier);
				BacktrackTo(backtrackPosition2);
				// Also optional, but this is the fallback if neither succeeds
				AstNode* braceOrEqualInitializer = ParseBraceOrEqualInitializer();
				return GenerateMemberDeclaratorBraceNode(declarator, virtSpecifierSeq, braceOrEqualInitializer);
			}
			FreeNode(declarator);
			BacktrackTo(backtrackPosition);

			// Optional
			Token identifier;
			identifier.m_Type = TokenType::None;
			if (Peek() == TokenType::IDENTIFIER)
			{
				identifier = ConsumeCurrent(TokenType::IDENTIFIER);
			}

			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			// Optional
			AstNode* virtSpecifierSeq = ParseVirtSpecifierSequence();
			if (!Match(TokenType::COLON))
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(virtSpecifierSeq);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			AstNode* constantExpression = ParseConstantExpression();
			if (!constantExpression->success)
			{
				FreeNode(attributeSpecifierSeq);
				FreeNode(virtSpecifierSeq);
				FreeNode(constantExpression);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemberDeclaratorNode(identifier, attributeSpecifierSeq, virtSpecifierSeq, constantExpression);
		}

		static AstNode* ParseVirtSpecifierSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseVirtSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextSpec = ParseVirtSpecifier();
				result = GenerateVirtSpecifierSeqNode(result, nextSpec);
				if (!nextSpec->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseVirtSpecifier()
		{
			if (Peek() == TokenType::KW_OVERRIDE || Peek() == TokenType::KW_FINAL || Peek() == TokenType::KW_NEW)
			{
				Token token = ConsumeCurrent(Peek());
				return GenerateVirtSpecifierNode(token);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParsePureSpecifier()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::EQUAL))
			{
				if (Peek() == TokenType::INTEGER_LITERAL)
				{
					Token token = ConsumeCurrent(TokenType::INTEGER_LITERAL);
					if (strcmp(token.m_Lexeme, "0") == 0)
					{
						return GeneratePureSpecifierNode();
					}
				}
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Derived classes
		static AstNode* ParseBaseClause()
		{
			if (Match(TokenType::COLON))
			{
				return ParseBaseSpecifierList();
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBaseSpecifierList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseBaseSpecifier();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(TokenType::COMMA))
			{
				AstNode* nextBase = ParseBaseSpecifierList();
				result = GenerateBaseSpecifierListNode(result, nextBase);
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return result;
		}

		static AstNode* ParseBaseSpecifier()
		{
			// TODO: this is weird, make sure I didn't goof up here
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			bool isVirtual = Match(TokenType::KW_VIRTUAL);

			int backtrackPosition2 = CurrentToken;
			AstNode* accessSpecifier = ParseAccessSpecifier();
			if (!accessSpecifier->success)
			{
				FreeNode(accessSpecifier);
				BacktrackTo(backtrackPosition2);
				accessSpecifier = GenerateNoSuccessAstNode();

				if (!isVirtual)
				{
					isVirtual = Match(TokenType::KW_VIRTUAL);
				}
			}

			AstNode* baseTypeSpecifier = ParseBaseTypeSpecifier();
			if (baseTypeSpecifier->success)
			{
				return GenerateBaseSpecifierNode(attributeSpecifierSeq, isVirtual, accessSpecifier, baseTypeSpecifier);
			}

			FreeNode(baseTypeSpecifier);
			FreeNode(accessSpecifier);
			FreeNode(attributeSpecifierSeq);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseClassOrDecltype()
		{
			int backtrackPosition = CurrentToken;
			AstNode* decltypeSpecifier = ParseDecltypeSpecifier();
			if (decltypeSpecifier->success)
			{
				return decltypeSpecifier;
			}
			FreeNode(decltypeSpecifier);
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::COLON))
			{
				Consume(TokenType::COLON);
			}

			// Optional
			AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
			if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
			{
				FreeNode(nestedNameSpecifier);
				nestedNameSpecifier = ParseNestedNameSpecifier();
			}
			AstNode* className = ParseClassName();
			if (className->success)
			{
				return GenerateClassOrDecltypeNode(nestedNameSpecifier, className);
			}

			FreeNode(nestedNameSpecifier);
			FreeNode(className);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseBaseTypeSpecifier()
		{
			return ParseClassOrDecltype();
		}

		static AstNode* ParseAccessSpecifier()
		{
			if (Peek() == TokenType::KW_PRIVATE || Peek() == TokenType::KW_PROTECTED || Peek() == TokenType::KW_PUBLIC)
			{
				Token accessSpecifier = ConsumeCurrent(Peek());
				return GenerateAccessSpecifierNode(accessSpecifier);
			}

			return GenerateNoSuccessAstNode();
		}

		// Class conversion functions
		static AstNode* ParseConversionFunctionId()
		{
			if (Match(TokenType::KW_OPERATOR))
			{
				AstNode* conversionTypeId = ParseConversionTypeId();
				return GenerateConversionFunctionIdNode(conversionTypeId);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseConversionTypeId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence();
			if (!typeSpecifierSeq->success)
			{
				FreeNode(typeSpecifierSeq);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* conversionDeclarator = ParseConversionDeclarator();
			return GenerateConversionTypeIdNode(typeSpecifierSeq, conversionDeclarator);
		}

		static AstNode* ParseConversionDeclarator()
		{
			int backtrackPosition = CurrentToken;
			AstNode* ptrOperator = ParsePtrOperator();
			if (!ptrOperator->success)
			{
				FreeNode(ptrOperator);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			// Optional
			AstNode* conversionDeclarator = ParseConversionDeclarator();
			return GenerateConversionDeclaratorNode(ptrOperator, conversionDeclarator);
		}

		// Class initializers
		static AstNode* ParseCtorInitializer()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::COLON))
			{
				AstNode* memInitializerList = ParseMemInitializerList();
				if (memInitializerList->success)
				{
					return memInitializerList;
				}
				FreeNode(memInitializerList);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseMemInitializerList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseMemInitializer();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(TokenType::COMMA))
			{
				AstNode* nextInitializer = ParseMemInitializer();
				result = GenerateMemInitializerListNode(result, nextInitializer);
				if (!nextInitializer->success)
				{
					break;
				}
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return result;
		}

		static AstNode* ParseMemInitializer()
		{
			int backtrackPosition = CurrentToken;
			AstNode* memInitializerId = ParseMemInitializerId();
			if (!memInitializerId->success)
			{
				FreeNode(memInitializerId);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			if (Match(TokenType::LEFT_PAREN))
			{
				// Optional
				AstNode* expressionList = ParseExpressionList();
				Consume(TokenType::RIGHT_PAREN);

				return GenerateMemExpressionInitializerNode(memInitializerId, expressionList);
			}

			AstNode* bracedInitList = ParseBracedInitList();
			if (!bracedInitList->success)
			{
				FreeNode(memInitializerId);
				FreeNode(bracedInitList);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			return GenerateMemBracedInitializerNode(memInitializerId, bracedInitList);
		}

		static AstNode* ParseMemInitializerId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* classOrDecltype = ParseClassOrDecltype();
			if (classOrDecltype->success)
			{
				return classOrDecltype;
			}
			FreeNode(classOrDecltype);
			BacktrackTo(backtrackPosition);

			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateMemInitializerIdNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		// Operator overloading
		static OverloadableOperatorType ParseOverloadableOperator()
		{
			if (Match(TokenType::KW_NEW))
			{
				if (Match(TokenType::LEFT_BRACKET))
				{
					Consume(TokenType::RIGHT_BRACKET);
					return OverloadableOperatorType::NewArr;
				}
				return  OverloadableOperatorType::New;
			}

			if (Match(TokenType::KW_DELETE))
			{
				if (Match(TokenType::LEFT_BRACKET))
				{
					Consume(TokenType::RIGHT_BRACKET);
					return OverloadableOperatorType::DeleteArr;
				}
				return OverloadableOperatorType::Delete;
			}

			Token token = ConsumeCurrent(Peek());
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
				if (Match(TokenType::STAR))
				{
					return OverloadableOperatorType::ArrowStar;
				}
				return OverloadableOperatorType::Arrow;
			}
			case TokenType::LEFT_PAREN:
				Consume(TokenType::RIGHT_PAREN);
				return OverloadableOperatorType::ParenGroup;
			case TokenType::LEFT_BRACKET:
				Consume(TokenType::RIGHT_BRACKET);
				return OverloadableOperatorType::BracketGroup;
			}

			return OverloadableOperatorType::None;
		}

		static AstNode* ParseOperatorFunctionId()
		{
			if (Match(TokenType::KW_OPERATOR))
			{
				OverloadableOperatorType op = ParseOverloadableOperator();
				if (Match(TokenType::LEFT_ANGLE_BRACKET))
				{
					AstNode* templateArgList = ParseTemplateArgumentList();
					Consume(TokenType::RIGHT_ANGLE_BRACKET);
					return GenerateOperatorFunctionIdNode(op, templateArgList);
				}

				return GenerateOperatorFunctionIdNode(op, GenerateNoSuccessAstNode());
			}

			return GenerateNoSuccessAstNode();
		}

		// Literal overrides
		static AstNode* ParseLiteralOperatorId()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_OPERATOR))
			{
				if (Peek() == TokenType::STRING_LITERAL)
				{
					Token token = ConsumeCurrent(TokenType::STRING_LITERAL);
					CPP_PARSER_LOG_ASSERT(ParserString::StringLength(token.m_Lexeme) == 0, "Invalid custom overloaded operator. Syntax is 'operator\"\" identifier'");
					if (Peek() == TokenType::IDENTIFIER)
					{
						Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
						return GenerateLiteralOperatorIdNode(identifier);
					}
				}
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Templates
		static AstNode* ParseTemplateDeclaration()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TEMPLATE))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				AstNode* templateParameterList = ParseTemplateParameterList();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				AstNode* declaration = ParseDeclaration();

				if (!(templateParameterList->success && declaration->success))
				{
					FreeNode(templateParameterList);
					FreeNode(declaration);
					BacktrackTo(backtrackPosition);
					return GenerateNoSuccessAstNode();
				}

				return GenerateTemplateDeclarationNode(templateParameterList, declaration);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateParameterList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseTemplateParameter();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(TokenType::COMMA))
			{
				result = GenerateTemplateParameterListNode(result, ParseTemplateParameterList());
			}

			return GenerateTemplateParameterListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseTemplateParameter()
		{
			int backtrackPosition = CurrentToken;
			AstNode* typeParameter = ParseTypeParameter();
			if (typeParameter->success)
			{
				return typeParameter;
			}
			FreeNode(typeParameter);
			BacktrackTo(backtrackPosition);

			AstNode* parameterDeclaration = ParseParameterDeclaration();
			if (parameterDeclaration->success)
			{
				return parameterDeclaration;
			}

			FreeNode(parameterDeclaration);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeParameter()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TEMPLATE))
			{
				if (Match(TokenType::LEFT_ANGLE_BRACKET))
				{
					AstNode* templateParameterList = ParseTemplateParameterList();
					if (templateParameterList->success)
					{
						if (Match(TokenType::KW_CLASS))
						{
							bool hasElipsis = Match(TokenType::DOT);
							if (hasElipsis)
							{
								Consume(TokenType::DOT);
								Consume(TokenType::DOT);
							}

							Token identifier;
							identifier.m_Type = TokenType::None;
							if (Peek() == TokenType::IDENTIFIER)
							{
								identifier = ConsumeCurrent(TokenType::IDENTIFIER);
							}

							if (Match(TokenType::EQUAL))
							{
								return GenerateTypeTemplateParameterNode(templateParameterList, identifier, ParseIdExpression());
							}
							return GenerateTypeTemplateParameterNode(templateParameterList, identifier, GenerateNoSuccessAstNode());
						}
					}
					FreeNode(templateParameterList);
				}
			}
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::KW_TYPENAME))
			{
				bool hasElipsis = Match(TokenType::DOT);
				if (hasElipsis)
				{
					Consume(TokenType::DOT);
					Consume(TokenType::DOT);
				}

				Token identifier;
				identifier.m_Type = TokenType::None;
				if (Peek() == TokenType::IDENTIFIER)
				{
					identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				}

				if (Match(TokenType::EQUAL))
				{
					return GenerateTypeTypenameParameterNode(identifier, ParseTypeId());
				}
				return GenerateTypeTypenameParameterNode(identifier, GenerateNoSuccessAstNode());
			}
			BacktrackTo(backtrackPosition);

			if (Match(TokenType::KW_CLASS))
			{
				bool hasElipsis = Match(TokenType::DOT);
				if (hasElipsis)
				{
					Consume(TokenType::DOT);
					Consume(TokenType::DOT);
				}

				Token identifier;
				identifier.m_Type = TokenType::None;
				if (Peek() == TokenType::IDENTIFIER)
				{
					identifier = ConsumeCurrent(TokenType::IDENTIFIER);
				}

				if (Match(TokenType::EQUAL))
				{
					return GenerateTypeClassParameterNode(identifier, ParseTypeId());
				}
				return GenerateTypeClassParameterNode(identifier, GenerateNoSuccessAstNode());
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseSimpleTemplateId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* templateName = ParseTemplateName();
			if (templateName->success)
			{
				if (Match(TokenType::LEFT_ANGLE_BRACKET))
				{
					// Optional
					AstNode* templateArgumentList = ParseTemplateArgumentList();
					if (Match(TokenType::RIGHT_ANGLE_BRACKET))
					{
						return GenerateSimpleTemplateIdNode(templateName, templateArgumentList);
					}
				}
			}

			FreeNode(templateName);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateId()
		{
			int backtrackPosition = CurrentToken;
			AstNode* simpleTemplateId = ParseSimpleTemplateId();
			if (simpleTemplateId->success)
			{
				return simpleTemplateId;
			}
			FreeNode(simpleTemplateId);
			BacktrackTo(backtrackPosition);

			AstNode* literalOperatorId = ParseLiteralOperatorId();
			if (literalOperatorId->success)
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				// Optional
				AstNode* templateArgumentList = ParseTemplateArgumentList();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				return GenerateLiteralOperatorTemplateIdNode(literalOperatorId, templateArgumentList);
			}
			FreeNode(literalOperatorId);
			BacktrackTo(backtrackPosition);

			AstNode* operatorFunctionId = ParseOperatorFunctionId();
			if (operatorFunctionId->success)
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				// Optional
				AstNode* templateArgumentList = ParseTemplateArgumentList();
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				return GenerateFunctionOperatorTemplateIdNode(operatorFunctionId, templateArgumentList);
			}
			FreeNode(operatorFunctionId);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateName()
		{
			if (Peek() == TokenType::IDENTIFIER)
			{
				return GenerateTemplateNameNode(ConsumeCurrent(TokenType::IDENTIFIER));
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTemplateArgumentList()
		{
			AstNode* result = ParseTemplateArgument();

			while (Match(TokenType::COMMA))
			{
				result = GenerateTemplateArgumentListNode(result, ParseTemplateArgumentList());
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return GenerateTemplateArgumentListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseTemplateArgument()
		{
			int backtrackPosition = CurrentToken;
			AstNode* idExpression = ParseIdExpression();
			if (idExpression->success)
			{
				return idExpression;
			}
			FreeNode(idExpression);
			BacktrackTo(backtrackPosition);

			AstNode* typeId = ParseTypeId();
			if (typeId->success)
			{
				return typeId;
			}
			FreeNode(typeId);
			BacktrackTo(backtrackPosition);

			AstNode* constantExpression = ParseConstantExpression();
			if (constantExpression->success)
			{
				return constantExpression;
			}
			FreeNode(constantExpression);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypenameSpecifier()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TYPENAME))
			{
				if (Match(TokenType::COLON))
				{
					Consume(TokenType::COLON);
				}

				AstNode* nestedNameSpecifier = GenerateNoSuccessAstNode();
				if (MatchBeforeSemicolon(TokenType::COLON, TokenType::COLON))
				{
					FreeNode(nestedNameSpecifier);
					nestedNameSpecifier = ParseNestedNameSpecifier();
				}
				if (nestedNameSpecifier->success)
				{
					if (Peek() == TokenType::IDENTIFIER)
					{
						return GenerateTypenameSpecifierNode(nestedNameSpecifier, ConsumeCurrent(TokenType::IDENTIFIER));
					}

					bool hasTemplateKeyword = Match(TokenType::KW_TEMPLATE);
					AstNode* simpleTemplateId = ParseSimpleTemplateId();
					if (simpleTemplateId->success)
					{
						return GenerateTypenameTemplateSpecifierNode(nestedNameSpecifier, simpleTemplateId, hasTemplateKeyword);
					}
					FreeNode(simpleTemplateId);
				}
				FreeNode(nestedNameSpecifier);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExplicitInstantiation()
		{
			int backtrackPosition = CurrentToken;
			bool hasExternKeyword = Match(TokenType::KW_EXTERN);
			if (Match(TokenType::KW_TEMPLATE))
			{
				AstNode* declaration = ParseDeclaration();
				if (declaration->success)
				{
					return GenerateExplicitInstantiationNode(declaration, hasExternKeyword);
				}
				FreeNode(declaration);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExplicitSpecialization()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TEMPLATE))
			{
				Consume(TokenType::LEFT_ANGLE_BRACKET);
				Consume(TokenType::RIGHT_ANGLE_BRACKET);
				AstNode* declaration = ParseDeclaration();
				if (declaration->success)
				{
					declaration;
				}
				FreeNode(declaration);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		// Exceptions
		static AstNode* ParseTryBlock()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TRY))
			{
				AstNode* compoundStatement = ParseCompoundStatement();
				if (compoundStatement->success)
				{
					AstNode* handlerSeq = ParseHandlerSequence();
					if (handlerSeq->success)
					{
						return GenerateTryBlockNode(compoundStatement, handlerSeq);
					}
					FreeNode(handlerSeq);
				}
				FreeNode(compoundStatement);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseFunctionTryBlock()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_TRY))
			{
				// Optional
				AstNode* ctorInitializer = ParseCtorInitializer();
				AstNode* compoundStatement = ParseCompoundStatement();
				if (compoundStatement->success)
				{
					AstNode* handlerSeq = ParseHandlerSequence();
					if (handlerSeq->success)
					{
						return GenerateFunctionTryBlockNode(ctorInitializer, compoundStatement, handlerSeq);
					}
					FreeNode(handlerSeq);
				}
				FreeNode(compoundStatement);
				FreeNode(ctorInitializer);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseHandlerSequence()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseHandler();
			if (!result->success)
			{
				BacktrackTo(backtrackPosition);
				FreeNode(result);
				return GenerateNoSuccessAstNode();
			}

			while (true)
			{
				AstNode* nextHandler = ParseHandlerSequence();
				result = GenerateHandlerSeqNode(result, nextHandler);
				if (!nextHandler->success)
				{
					break;
				}
			}

			return result;
		}

		static AstNode* ParseHandler()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_CATCH))
			{
				Consume(TokenType::LEFT_PAREN);
				AstNode* exceptionDeclaration = ParseExceptionDeclaration();
				if (exceptionDeclaration->success)
				{
					Consume(TokenType::RIGHT_PAREN);
					AstNode* compoundStatement = ParseCompoundStatement();
					if (compoundStatement->success)
					{
						return GenerateHandlerNode(exceptionDeclaration, compoundStatement);
					}
					FreeNode(compoundStatement);
				}
				FreeNode(exceptionDeclaration);
			}

			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExceptionDeclaration()
		{
			int backtrackPosition = CurrentToken;
			// Optional
			AstNode* attributeSpecifierSeq = ParseAttributeSpecifierSequence();
			AstNode* typeSpecifierSeq = ParseTypeSpecifierSequence();
			if (typeSpecifierSeq->success)
			{
				int backtrackPosition2 = CurrentToken;
				AstNode* declarator = ParseDeclarator();
				if (declarator->success)
				{
					return GenerateExceptionDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
				}
				FreeNode(declarator);
				BacktrackTo(backtrackPosition2);

				// Optional
				AstNode* abstractDeclarator = ParseAbstractDeclarator();
				return GenerateExceptionAbstractDeclarationNode(attributeSpecifierSeq, typeSpecifierSeq, declarator);
			}

			FreeNode(attributeSpecifierSeq);
			FreeNode(typeSpecifierSeq);
			BacktrackTo(backtrackPosition);
			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseThrowExpression()
		{
			if (Match(TokenType::KW_THROW))
			{
				// Optional
				AstNode* assignmentExpression = ParseAssignmentExpression();
				return GenerateThrowExpressionNode(assignmentExpression);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseExceptionSpecification()
		{
			int backtrackPosition = CurrentToken;
			AstNode* noexceptSpecification = ParseNoexceptSpecification();
			if (noexceptSpecification->success)
			{
				return noexceptSpecification;
			}
			FreeNode(noexceptSpecification);
			BacktrackTo(backtrackPosition);

			AstNode* dynamicExceptionSpecification = ParseDynamicExceptionSpecification();
			if (dynamicExceptionSpecification->success)
			{
				return dynamicExceptionSpecification;
			}
			FreeNode(dynamicExceptionSpecification);
			BacktrackTo(backtrackPosition);

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseDynamicExceptionSpecification()
		{
			if (Match(TokenType::KW_THROW))
			{
				Consume(TokenType::LEFT_PAREN);
				// Optional
				AstNode* typeIdList = ParseTypeIdList();
				Consume(TokenType::RIGHT_PAREN);

				return GenerateDynamicExceptionSpecNode(typeIdList);
			}

			return GenerateNoSuccessAstNode();
		}

		static AstNode* ParseTypeIdList()
		{
			int backtrackPosition = CurrentToken;
			AstNode* result = ParseTypeId();
			if (!result->success)
			{
				FreeNode(result);
				BacktrackTo(backtrackPosition);
				return GenerateNoSuccessAstNode();
			}

			while (Match(TokenType::COMMA))
			{
				result = GenerateTypeIdListNode(result, ParseTypeIdList());
			}

			if (Match(TokenType::DOT))
			{
				Consume(TokenType::DOT);
				Consume(TokenType::DOT);
			}

			return GenerateTypeIdListNode(result, GenerateNoSuccessAstNode());
		}

		static AstNode* ParseNoexceptSpecification()
		{
			int backtrackPosition = CurrentToken;
			if (Match(TokenType::KW_NOEXCEPT))
			{
				if (Match(TokenType::LEFT_PAREN))
				{
					AstNode* constantExpression = ParseConstantExpression();
					if (constantExpression->success)
					{
						Consume(TokenType::RIGHT_PAREN);
						return GenerateNoexceptExpressionSpecNode(constantExpression);
					}
					FreeNode(constantExpression);
					return GenerateNoSuccessAstNode();
				}

				return GenerateNoexceptSpecNode();
			}

			return GenerateNoSuccessAstNode();
		}

		// Preprocessing Stuff
		// TODO: Most of this works, but if I want to use it I must create a preprocessing engine
		//static AstNode* ParsePreprocessingFile()
		//{
		//	return GeneratePreprocessingFileNode(ParseGroup());
		//}

		//static AstNode* ParseGroup()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* result = ParseGroupPart();
		//	if (!result->success)
		//	{
		//		FreeNode(result);
		//		BacktrackTo(backtrackPosition);
		//	}

		//	while (true)
		//	{
		//		AstNode* nextGroup = ParseGroup();
		//		result = GenerateGroupNode(result, nextGroup);
		//		if (!nextGroup->success)
		//		{
		//			break;
		//		}
		//	}

		//	return result;
		//}

		//static AstNode* ParseGroupPart()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* ifSection = ParseIfSection();
		//	if (ifSection->success)
		//	{
		//		return ifSection;
		//	}
		//	FreeNode(ifSection);
		//	BacktrackTo(backtrackPosition);

		//	AstNode* controlLine = ParseControlLine();
		//	if (controlLine->success)
		//	{
		//		return controlLine;
		//	}
		//	FreeNode(controlLine);
		//	BacktrackTo(backtrackPosition);

		//	AstNode* textLine = ParseTextLine();
		//	if (textLine->success)
		//	{
		//		return textLine;
		//	}
		//	FreeNode(textLine);
		//	BacktrackTo(backtrackPosition);

		//	// TODO: Add support for non-directives they look like:
		//	// TODO: #
		//	// TODO: a pound symbol followed by a newline
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseIfSection()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* ifGroup = ParseIfGroup();
		//	if (ifGroup->success)
		//	{
		//		// Optional
		//		AstNode* elifGroups = ParseElifGroups();
		//		AstNode* elseGroup = ParseElseGroup();
		//		Consume(TokenType::MACRO_ENDIF);
		//		return GenerateIfSectionNode(ifGroup, elifGroups, elseGroup);
		//	}

		//	FreeNode(ifGroup);
		//	BacktrackTo(backtrackPosition);
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseIfGroup()
		//{
		//	int backtrackPosition = CurrentToken;
		//	if (Match(TokenType::MACRO_IF))
		//	{
		//		AstNode* constantExpression = ParseConstantExpression();
		//		if (constantExpression->success)
		//		{
		//			// TODO: Consume a new line here?
		//			// optional
		//			AstNode* group = ParseGroup();
		//			return GenerateIfGroupNode(constantExpression, group);
		//		}
		//		FreeNode(constantExpression);
		//	}
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::MACRO_IFDEF))
		//	{
		//		if (Peek() == TokenType::IDENTIFIER)
		//		{
		//			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
		//			// TODO: consume newline here
		//			// Optional
		//			AstNode* group = ParseGroup();
		//			return GenerateIfDefGroupNode(identifier, group);
		//		}
		//	}

		//	if (Match(TokenType::MACRO_IFNDEF))
		//	{
		//		if (Peek() == TokenType::IDENTIFIER)
		//		{
		//			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
		//			// TODO: consume newline here
		//			// Optional
		//			AstNode* group = ParseGroup();
		//			return GenerateIfNDefGroupNode(identifier, group);
		//		}
		//	}

		//	BacktrackTo(backtrackPosition);
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseElifGroups()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* result = ParseElifGroup();
		//	if (!result->success)
		//	{
		//		FreeNode(result);
		//		BacktrackTo(backtrackPosition);
		//		return GenerateNoSuccessAstNode();
		//	}

		//	while (true)
		//	{
		//		AstNode* nextGroup = ParseElifGroups();
		//		result = GenerateElifGroupsNode(result, nextGroup);
		//		if (!nextGroup->success)
		//		{
		//			break;
		//		}
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseElifGroup()
		//{
		//	int backtrackPosition = CurrentToken;
		//	if (Match(TokenType::MACRO_ELIF))
		//	{
		//		AstNode* constantExpression = ParseConstantExpression();
		//		if (constantExpression->success)
		//		{
		//			// TODO: consume newline
		//			// Optional
		//			AstNode* group = ParseGroup();
		//			return GenerateElifGroupNode(constantExpression, group);
		//		}
		//		FreeNode(constantExpression);
		//	}

		//	BacktrackTo(backtrackPosition);
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseElseGroup()
		//{
		//	int backtrackPosition = CurrentToken;
		//	if (Match(TokenType::MACRO_ELSE))
		//	{
		//		// TODO: consume newline
		//		// Optional
		//		AstNode* group = ParseGroup();
		//		return GenerateElseGroupNode(group);
		//	}

		//	BacktrackTo(backtrackPosition);
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseControlLine()
		//{
		//	int backtrackPosition = CurrentToken;
		//	if (Match(TokenType::MACRO_INCLUDE))
		//	{
		//		if (Match(TokenType::LEFT_ANGLE_BRACKET))
		//		{
		//			AstNode* ppTokens = ParsePPTokens();
		//			if (ppTokens->success)
		//			{
		//				Consume(TokenType::RIGHT_ANGLE_BRACKET);
		//				// TODO: Consume newline
		//				return GenerateMacroIncludeNode(ppTokens);
		//			}
		//			FreeNode(ppTokens);
		//		}
		//		else
		//		{
		//			AstNode* ppTokens = ParsePPTokens();
		//			if (ppTokens->success)
		//			{
		//				// TODO: Consume newline
		//				return GenerateMacroIncludeNode(ppTokens);
		//			}
		//			FreeNode(ppTokens);
		//		}
		//	}
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::MACRO_DEFINE))
		//	{
		//		if (Peek() == TokenType::IDENTIFIER)
		//		{
		//			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
		//			// TODO: make sure this left parenthisis is not preceded by whitespace
		//			if (Match(TokenType::LEFT_PAREN))
		//			{
		//				AstNode* identifierList = ParseIdentifierList();
		//				if (identifierList->success)
		//				{
		//					Match(TokenType::COMMA);
		//					if (Match(TokenType::DOT))
		//					{
		//						Consume(TokenType::DOT);
		//						Consume(TokenType::DOT);
		//					}
		//				}

		//				Consume(TokenType::RIGHT_PAREN);
		//				AstNode* replacementList = ParseReplacementList();
		//				if (replacementList->success)
		//				{
		//					// TODO: Consume newline
		//					return GenerateMacroDefineFunctionNode(identifier, identifierList, replacementList);
		//				}
		//				FreeNode(replacementList);
		//				FreeNode(identifierList);
		//			}

		//			AstNode* replacementList = ParseReplacementList();
		//			if (replacementList->success)
		//			{
		//				// TODO: consume newline
		//				return GenerateMacroDefineNode(identifier, replacementList);
		//			}
		//			FreeNode(replacementList);
		//		}
		//	}
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::MACRO_UNDEF))
		//	{
		//		if (Peek() == TokenType::IDENTIFIER)
		//		{
		//			Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
		//			// TODO: Consume newline
		//			return GenerateMacroUndefNode(identifier);
		//		}
		//	}
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::MACRO_LINE))
		//	{
		//		AstNode* ppTokens = ParsePPTokens();
		//		if (ppTokens->success)
		//		{
		//			// TODO: consume newline
		//			return GenerateMacroLineNode(ppTokens);
		//		}
		//		FreeNode(ppTokens);
		//	}
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::MACRO_ERROR))
		//	{
		//		// Optional
		//		AstNode* ppTokens = ParsePPTokens();
		//		// TODO: Consume newline
		//		return GenerateMacroErrorNode(ppTokens);
		//	}

		//	if (Match(TokenType::MACRO_PRAGMA))
		//	{
		//		// Optional
		//		AstNode* ppTokens = ParsePPTokens();
		//		// TODO: consume newline
		//		return GenerateMacroPragmaNode(ppTokens);
		//	}

		//	// TODO: Consume # symbol followed by newline
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseTextLine()
		//{
		//	// Optional
		//	AstNode* ppTokens = ParsePPTokens();
		//	// TODO: Consume newline
		//	return GenerateTextLineNode(ppTokens);
		//}

		//static AstNode* ParseNonDirective()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* ppTokens = ParsePPTokens();
		//	if (ppTokens->success)
		//	{
		//		// TODO: Consume newline
		//		return GenerateNonDirectiveNode(ppTokens);
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseIdentifierList()
		//{
		//	if (Peek() == TokenType::IDENTIFIER)
		//	{
		//		AstNode* result = GenerateIdentifierNode(ConsumeCurrent(TokenType::IDENTIFIER));

		//		while (Match(TokenType::COMMA))
		//		{
		//			result = GenerateIdentifierListNode(result, ParseIdentifierList());
		//		}

		//		return result;
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseReplacementList()
		//{
		//	// Optional
		//	AstNode* ppTokens = ParsePPTokens();
		//	return GenerateReplacementListNode(ppTokens);
		//}

		//static AstNode* ParsePPTokens()
		//{
		//	int backtrackPosition = CurrentToken;
		//	AstNode* result = ParsePreprocessingToken();
		//	if (!result->success)
		//	{
		//		FreeNode(result);
		//		BacktrackTo(backtrackPosition);
		//		return GenerateNoSuccessAstNode();
		//	}

		//	while (true)
		//	{
		//		AstNode* nextPPToken = ParsePPTokens();
		//		result = GeneratePPTokensNode(result, nextPPToken);
		//		if (!nextPPToken->success)
		//		{
		//			break;
		//		}
		//	}

		//	return result;
		//}

		//static AstNode* ParseNumberLiteral()
		//{
		//	if (Peek() == TokenType::FLOATING_POINT_LITERAL || Peek() == TokenType::INTEGER_LITERAL)
		//	{
		//		return GenerateNumberLiteralNode(ConsumeCurrent(Peek()));
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		//// Preprocessor Stuff
		//static AstNode* ParsePreprocessingToken()
		//{
		//	//if (Match(TokenType::NEWLINE))
		//	//{
		//	//	return GenerateNoSuccessAstNode();
		//	//}

		//	int backtrackPosition = CurrentToken;
		//	AstNode* headerName = ParseHeaderName();
		//	if (headerName->success)
		//	{
		//		return headerName;
		//	}
		//	FreeNode(headerName);
		//	BacktrackTo(backtrackPosition);

		//	if (Peek() == TokenType::IDENTIFIER)
		//	{
		//		return GenerateIdentifierNode(ConsumeCurrent(TokenType::IDENTIFIER));
		//	}

		//	AstNode* numberLiteral = ParseNumberLiteral();
		//	if (numberLiteral->success)
		//	{
		//		return numberLiteral;
		//	}
		//	FreeNode(numberLiteral);
		//	BacktrackTo(backtrackPosition);

		//	AstNode* characterLiteral = ParseCharacterLiteral();
		//	if (characterLiteral->success)
		//	{
		//		return characterLiteral;
		//	}
		//	FreeNode(characterLiteral);
		//	BacktrackTo(backtrackPosition);

		//	AstNode* stringLiteral = ParseStringLiteral();
		//	if (stringLiteral->success)
		//	{
		//		return stringLiteral;
		//	}
		//	FreeNode(stringLiteral);
		//	BacktrackTo(backtrackPosition);

		//	if (Match(TokenType::DOT))
		//	{
		//		return GenerateEmptyStatementNode();
		//	}

		//	// TODO: Should I do this...?
		//	//AstNode* preprocessingOpOrPunc = ParsePreprocessingOpOrPunc();
		//	//if (preprocessingOpOrPunc->success)
		//	//{
		//	//	return preprocessingOpOrPunc;
		//	//}
		//	//FreeNode(preprocessingOpOrPunc);
		//	//BacktrackTo(backtrackPosition);

		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseHeaderName()
		//{
		//	if (Peek() == TokenType::LEFT_ANGLE_BRACKET)
		//	{
		//		Token identifier = ConsumeCurrent(TokenType::IDENTIFIER);
		//		Consume(TokenType::RIGHT_ANGLE_BRACKET);
		//		return GenerateHeaderNameNode(identifier);
		//	}

		//	if (Peek() == TokenType::STRING_LITERAL)
		//	{
		//		Token stringLiteral = ConsumeCurrent(TokenType::STRING_LITERAL);
		//		return GenerateHeaderNameStringNode(stringLiteral);
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseCharacterLiteral()
		//{
		//	if (Peek() == TokenType::CHARACTER_LITERAL)
		//	{
		//		return GenerateCharacterLiteralNode(ConsumeCurrent(TokenType::CHARACTER_LITERAL));
		//	}
		//	return GenerateNoSuccessAstNode();
		//}

		//static AstNode* ParseStringLiteral()
		//{
		//	if (Peek() == TokenType::STRING_LITERAL)
		//	{
		//		Token stringLiteral = ConsumeCurrent(TokenType::STRING_LITERAL);
		//		return GenerateStringLiteralNode(stringLiteral);
		//	}

		//	return GenerateNoSuccessAstNode();
		//}

		// TODO: Should I do these...?
		// static AstNode* ParsePreprocessingOpOrPunc();
		// static AstNode* ParseHCharSequence();
		// static AstNode* ParseHChar();
		// static AstNode* ParseQCharSequence();
		// static AstNode* ParseQChar();
	}
}