@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pur. Requisition - Cat. Valoración x Cuenta'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZMM_R_SOLPED_CATVAL_CTA
  as select from ztb_a_catval_cta as CatValCta
{
  key CatValCta.bklas                 as ValuationClass,
      CatValCta.sakto                 as GLAccount,
      @Semantics.user.createdBy: true
      CatValCta.local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      CatValCta.local_created_at      as LocalCreatedAt,
      @Semantics.user.localInstanceLastChangedBy: true
      CatValCta.local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      CatValCta.local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      CatValCta.last_changed_at       as LastChangedAt
}
