@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Pur.Req. - Cat. Valoración x Cuenta'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: [ 'ValuationClass' ]
@Search.searchable: true
define root view entity ZMM_C_SOLPED_CATVAL_CTA
  provider contract transactional_query
  as projection on ZMM_R_SOLPED_CATVAL_CTA
{
      @Search.defaultSearchElement: true
  key ValuationClass,
      GLAccount,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt
}
