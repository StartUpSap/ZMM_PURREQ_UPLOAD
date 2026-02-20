@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Solped Header'
@Metadata.ignorePropagatedAnnotations: true
define root view entity zmm_r_solped_header
  as select from ztb_a_solped_hea as Header
  composition [0..*] of ZMM_i_SOLPED_detail as _Detail
{
  key Header.header_id             as HeaderId,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'Filename',
      contentDispositionPreference: #INLINE }
      attachment                   as Attachment,
      @Semantics.mimeType: true
      mimetype                     as MimeType,
      filename                     as Filename,
      Header.banfn                 as PurchaseRequisition,
      Header.bsart                 as PurchaseRequisitionType,
      @Semantics.user.createdBy: true
      Header.local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      Header.local_created_at      as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      Header.local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      Header.local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      Header.last_changed_at       as LastChangedAt,

      _Detail
}
