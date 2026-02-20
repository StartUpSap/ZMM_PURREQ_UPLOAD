@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solped Detail'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'HeaderId', 'DetailId' ]
@Search.searchable: true
define view entity ZMM_c_SOLPED_DETAIL
  as projection on ZMM_i_SOLPED_detail
{
  key HeaderId,
  key DetailId,
      @Search.defaultSearchElement: true
      PurchaseRequisition,
      PurchaseRequisitionItem,
      PurchasingOrganization,
      PurchasingGroup,
      RequisitionerName,
      Plant,
      StorageLocation,
      AccountAssignmentCategory,
      PurchasingDocumentItemCategory,
      @Search.defaultSearchElement: true
      Material,
      MaterialDescription,
      MaterialGroup,
      RequestedQuantity,
      BaseUnit,
      DeliveryDate,
      RequirementTracking,
      ItemText,
      PurchaseRequisitionPrice,
      PurReqnItemCurrency, //2846005
      GLAccount,
      CostCenter,
      PerformancePeriodStartDate,
      PerformancePeriodEndDate,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      /* Associations */
      _Header : redirected to parent ZMM_C_SOLPED_HEADER,
      _UnitOfMeasure
}
