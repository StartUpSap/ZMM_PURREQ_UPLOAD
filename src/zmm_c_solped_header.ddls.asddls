@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solped Header'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'HeaderId' ]
@Search.searchable: true
define root view entity ZMM_C_SOLPED_HEADER
  provider contract transactional_query
  as projection on zmm_r_solped_header
{
  key HeaderId,
      Attachment,
      MimeType,
      Filename,
      @Search.defaultSearchElement: true
      PurchaseRequisition,
      PurchaseRequisitionType,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,
      /* Associations */
      _Detail : redirected to composition child ZMM_c_SOLPED_DETAIL
}
