@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solped Detail'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZMM_i_SOLPED_detail
  as select from ztb_a_solped_det as Detail
  association        to parent zmm_r_solped_header as _Header             on $projection.HeaderId = _Header.HeaderId
  association [1..1] to I_UnitOfMeasure            as _UnitOfMeasure      on $projection.BaseUnit = _UnitOfMeasure.UnitOfMeasure
  association [1..1] to I_Product                  as _Product            on $projection.Material = _Product.Product
  association [0..*] to I_ProductDescription       as _ProductDescription on $projection.Material = _ProductDescription.Product
  association [0..1] to I_Currency                 as _Currency           on $projection.PurReqnItemCurrency = _Currency.Currency
{
  key Detail.header_id                                                               as HeaderId,
  key Detail.detail_id                                                               as DetailId,
      Detail.banfn                                                                   as PurchaseRequisition,
      Detail.bnfpo                                                                   as PurchaseRequisitionItem,
      Detail.ekorg                                                                   as PurchasingOrganization,
      Detail.ekgrp                                                                   as PurchasingGroup,
      Detail.afnam                                                                   as RequisitionerName,
      Detail.werks                                                                   as Plant,
      Detail.lgort                                                                   as StorageLocation,
      Detail.knttp                                                                   as AccountAssignmentCategory,
      Detail.pstyp                                                                   as PurchasingDocumentItemCategory,
      @ObjectModel.text.element: [ 'MaterialDescription' ]
      Detail.matnr                                                                   as Material,
      _ProductDescription[1: Language = $session.system_language].ProductDescription as MaterialDescription,
      _Product.ProductGroup                                                          as MaterialGroup,
      @Semantics.quantity.unitOfMeasure: 'BaseUnit'
      @DefaultAggregation: #NONE
      Detail.menge                                                                   as RequestedQuantity,
      @ObjectModel.foreignKey.association: '_UnitOfMeasure'
      Detail.meins                                                                   as BaseUnit,
      Detail.lfdat                                                                   as DeliveryDate,
      Detail.bednr                                                                   as RequirementTracking,
      Detail.text                                                                    as ItemText,
      @Semantics.amount.currencyCode: 'PurReqnItemCurrency'
      @DefaultAggregation: #NONE
      Detail.preis                                                                   as PurchaseRequisitionPrice,
      @ObjectModel.foreignKey.association: '_Currency'
      Detail.waers                                                                   as PurReqnItemCurrency, //2846005
      Detail.sakto                                                                   as GLAccount,
      Detail.kostl                                                                   as CostCenter,
      Detail.startdate                                                               as PerformancePeriodStartDate,
      Detail.enddate                                                                 as PerformancePeriodEndDate,
      @Semantics.user.createdBy: true
      Detail.local_created_by                                                        as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      Detail.local_created_at                                                        as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      Detail.local_last_changed_by                                                   as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      Detail.local_last_changed_at                                                   as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      Detail.last_changed_at                                                         as LastChangedAt,

      _Header,
      _UnitOfMeasure,
      _Currency,
      _Product,
      _ProductDescription
}
