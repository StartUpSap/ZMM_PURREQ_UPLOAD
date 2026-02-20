@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Account Assignment Category Value Help'
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zmm_AccAssignmentCategory_vh
  as select from I_AccountAssignmentCategory
{
      @ObjectModel.text.element:['AcctAssignmentCategoryName']
  key AccountAssignmentCategory,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @Semantics.text:true
      _Text[ 1: Language = $session.system_language ].AcctAssignmentCategoryName
}
