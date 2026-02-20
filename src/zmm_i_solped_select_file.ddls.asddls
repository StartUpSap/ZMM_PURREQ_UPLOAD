@EndUserText.label: 'Solped seleccionar archivo'
define abstract entity zmm_i_solped_select_file
{
  @EndUserText.label: 'Archivo'
  @Semantics.largeObject:
               { mimeType : 'MimeType',
  fileName   : 'Filename',
  contentDispositionPreference: #INLINE }
  @UI:
  { fieldGroup: [ { position: 10 , label: 'Attachment'} ]}
  attachment : abap.rawstring(0);
  @Semantics.mimeType: true
  @UI.hidden : true
  mimetype   : abap.char(128);
  @UI.hidden : true
  filename   : abap.char(128);

}
