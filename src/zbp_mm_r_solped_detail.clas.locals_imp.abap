CLASS lhc_detail DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE detail.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE detail.

    METHODS read FOR READ
      IMPORTING keys FOR READ detail RESULT result.

    METHODS rba_header FOR READ
      IMPORTING keys_rba FOR READ detail\_header FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_detail IMPLEMENTATION.

  METHOD update.
    zmm_solped_api_class=>get_instance( )->detail_update(
     EXPORTING
       entities = entities
     CHANGING
       mapped   = mapped
       failed   = failed
       reported = reported
   ).
  ENDMETHOD.

  METHOD delete.
    zmm_solped_api_class=>get_instance( )->detail_delete(
      EXPORTING
        keys     =  keys
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_header.
  ENDMETHOD.

ENDCLASS.
