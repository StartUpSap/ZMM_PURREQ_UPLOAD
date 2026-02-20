CLASS lhc_header DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR header RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR header RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR header RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE header.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE header.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE header.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE header.

    METHODS read FOR READ
      IMPORTING keys FOR READ header RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK header.

    METHODS earlynumbering_cba_detail FOR NUMBERING
      IMPORTING entities FOR CREATE header\_detail.

    METHODS rba_detail FOR READ
      IMPORTING keys_rba FOR READ header\_detail FULL result_requested RESULT result LINK association_links.

    METHODS cba_detail FOR MODIFY
      IMPORTING entities_cba FOR CREATE header\_detail.

    METHODS actioncreatepurchasereq FOR MODIFY
      IMPORTING keys FOR ACTION header~actioncreatepurchasereq RESULT result.

    METHODS filldatafromfile FOR DETERMINE ON MODIFY
      IMPORTING keys FOR header~filldatafromfile.
ENDCLASS.

CLASS lhc_header IMPLEMENTATION.

  METHOD get_instance_features.
    READ ENTITIES OF zmm_r_solped_header IN LOCAL MODE
    ENTITY header
    ALL FIELDS
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_header).

    result = VALUE #(
        FOR ls_header IN lt_header (
            %tky = ls_header-%tky
            %features-%update = COND #( WHEN ( ls_header-purchaserequisition = space )
                                        THEN if_abap_behv=>fc-o-enabled
                                        ELSE if_abap_behv=>fc-o-disabled )

*            %features-%delete = COND #( WHEN ( ls_header-%is_draft = '00' )
*                                        THEN COND #( WHEN ( ls_header-purchaserequisition = space )
*                                                     THEN if_abap_behv=>fc-o-enabled
*                                                     ELSE if_abap_behv=>fc-o-disabled )
*                                        ELSE if_abap_behv=>fc-o-disabled )

            %features-%action-actioncreatepurchasereq = COND #( WHEN ( ls_header-%is_draft = '00' )
                                                               THEN COND #( WHEN ( ls_header-purchaserequisition = space )
                                                                            THEN if_abap_behv=>fc-o-enabled
                                                                            ELSE if_abap_behv=>fc-o-disabled )
                                                               ELSE if_abap_behv=>fc-o-disabled )
        )
    ).
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD create.
    zmm_solped_api_class=>get_instance( )->header_create(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD earlynumbering_create.
    zmm_solped_api_class=>get_instance( )->earlynumbering_create_header(
      EXPORTING
        entities =  entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD update.
    zmm_solped_api_class=>get_instance( )->header_update(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD delete.
    zmm_solped_api_class=>get_instance( )->header_delete(
      EXPORTING
        keys     = keys
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD read.
    zmm_solped_api_class=>get_instance( )->header_read(
      EXPORTING
        keys     = keys
      CHANGING
        result   = result
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD earlynumbering_cba_detail.
    zmm_solped_api_class=>get_instance(  )->earlynumbering_cba_detail(
      EXPORTING
        entities = entities
      CHANGING
        mapped   = mapped
        failed   = failed
        reported = reported
    ).
  ENDMETHOD.

  METHOD rba_detail.
    zmm_solped_api_class=>get_instance( )->detail_read(
      EXPORTING
        keys_rba          = keys_rba
      CHANGING
        result            = result
        failed            = failed
        reported          = reported
    ).
  ENDMETHOD.

  METHOD cba_detail.
    zmm_solped_api_class=>get_instance(  )->detail_create(
      EXPORTING
        entities_cba = entities_cba
      CHANGING
        mapped       = mapped
        failed       = failed
        reported     = reported
    ).
  ENDMETHOD.

  METHOD actioncreatepurchasereq.
    DATA: lt_reported_pr TYPE RESPONSE FOR REPORTED EARLY i_purchaserequisitiontp,
          ls_mapped_pr   TYPE RESPONSE FOR MAPPED EARLY i_purchaserequisitiontp,
          ls_failed_pr   TYPE RESPONSE FOR FAILED EARLY i_purchaserequisitiontp,
          ls_reported_pr TYPE RESPONSE FOR REPORTED EARLY i_purchaserequisitiontp.

    zmm_solped_api_class=>get_instance(  )->create_purchase_requisition(
      EXPORTING
        keys        = keys
      CHANGING
        mapped_pr   = ls_mapped_pr
        failed_pr   = ls_failed_pr
        reported_pr = ls_reported_pr
    ).

    READ ENTITIES OF zmm_r_solped_header IN LOCAL MODE
    ENTITY header
    ALL FIELDS
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_header).

    LOOP AT ls_mapped_pr-purchaserequisition INTO DATA(ls_purchaserequisition).
      DATA(lv_purchaserequisition) = ls_purchaserequisition-purchaserequisition.
    ENDLOOP.

*   Actualizar datos de Header
    LOOP AT lt_header INTO DATA(ls_header).
      MODIFY ENTITIES OF zmm_r_solped_header IN LOCAL MODE
      ENTITY header
      UPDATE FROM VALUE #( FOR key IN keys
                           WHERE ( headerid = ls_header-headerid )
                           ( headerid = key-headerid
                             purchaserequisition = lv_purchaserequisition
                             %control-purchaserequisition = if_abap_behv=>mk-on  ) )
      FAILED DATA(lo_failed)
      REPORTED DATA(lo_reported).
    ENDLOOP.

    result = VALUE #( FOR ls_header_aux IN lt_header
                    ( %tky   = ls_header_aux-%tky
                      %param = ls_header_aux ) ).
  ENDMETHOD.

*  METHOD actionuploadfile.
*    LOOP AT keys INTO DATA(key).
*      IF key-%param-attachment IS INITIAL.
*        INSERT VALUE #(
*          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
*                                        text = 'Es necesario indicar el Archivo a cargar.' )
*        ) INTO TABLE reported-header.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.

  METHOD filldatafromfile.
    DATA: lt_create_detail TYPE TABLE FOR CREATE zmm_r_solped_header\\header\_detail.

    DATA: lt_detail TYPE STANDARD TABLE OF ztb_a_solped_det.

    DATA: lv_matnr18(18) TYPE c.

    GET TIME STAMP FIELD DATA(lv_timestamp).

    READ ENTITIES OF zmm_r_solped_header IN LOCAL MODE
    ENTITY header
    ALL FIELDS
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_header).

    IF NOT lt_header IS INITIAL.
      SELECT campohomo, camposap
      FROM ztablahomoum
      WHERE campohomo NE @space
      INTO TABLE @DATA(lt_homoum).

      LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<lfs_header>).

        IF NOT <lfs_header>-attachment IS INITIAL.
          DATA(lv_attachment) = <lfs_header>-attachment.
          DATA lv_requestedquantity TYPE string.
          DATA lv_purchaserequisitionprice TYPE string.

          DATA: lv_row     TYPE string,
                lt_rows    TYPE STANDARD TABLE OF string,
                lv_content TYPE string.

          DATA: lv_string TYPE string.

          TRY.
              lv_content = cl_abap_conv_codepage=>create_in( codepage = 'UTF-8' )->convert( lv_attachment )." default is 'UTF-8'
            CATCH cx_root.
              CLEAR lv_content.
          ENDTRY.

          SPLIT lv_content AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_rows.

          LOOP AT lt_rows INTO lv_row.
            AT FIRST.
              CONTINUE.
            ENDAT.

            APPEND INITIAL LINE TO lt_detail ASSIGNING FIELD-SYMBOL(<lfs_detail>).
            <lfs_detail>-header_id = <lfs_header>-headerid.
            <lfs_detail>-detail_id = sy-tabix.

            SPLIT lv_row AT ',' INTO <lfs_header>-purchaserequisitiontype
                                     <lfs_detail>-ekorg
                                     <lfs_detail>-ekgrp
                                     <lfs_detail>-afnam
                                     <lfs_detail>-werks
                                     <lfs_detail>-lgort
                                     <lfs_detail>-knttp
                                     <lfs_detail>-pstyp
                                     <lfs_detail>-matnr
                                     lv_requestedquantity
                                     <lfs_detail>-meins
                                     <lfs_detail>-lfdat
                                     <lfs_detail>-bednr
                                     <lfs_detail>-text
                                     lv_purchaserequisitionprice
                                     <lfs_detail>-waers
*                                     <lfs_detail>-sakto
                                     <lfs_detail>-kostl
                                     <lfs_detail>-startdate
                                     <lfs_detail>-enddate.

            lv_matnr18 = |{ <lfs_detail>-matnr ALPHA = IN }|.
            <lfs_detail>-matnr = lv_matnr18.

            <lfs_detail>-kostl = |{ <lfs_detail>-kostl ALPHA = IN }|.

            <lfs_detail>-menge = lv_requestedquantity.
            <lfs_detail>-preis = lv_purchaserequisitionprice.

            <lfs_detail>-local_created_by = sy-uname.
            <lfs_detail>-local_created_at = lv_timestamp.
            <lfs_detail>-local_last_changed_by = sy-uname.
            <lfs_detail>-local_last_changed_at = lv_timestamp.
            <lfs_detail>-last_changed_at = lv_timestamp.

            SELECT SINGLE valuationclass FROM i_productvaluationbasic
            WHERE product = @<lfs_detail>-matnr
            INTO @DATA(lv_productvaluationbasic).

            IF sy-subrc EQ 0.
              IF NOT lv_productvaluationbasic IS INITIAL.

                SELECT SINGLE sakto FROM ztb_a_catval_cta
                WHERE bklas = @lv_productvaluationbasic
                INTO @DATA(lv_sakto).

                IF sy-subrc EQ 0.
                  <lfs_detail>-sakto = lv_sakto.
                ENDIF.
              ENDIF.
            ENDIF.

            READ TABLE lt_homoum INTO DATA(ls_homoum) WITH KEY campohomo = <lfs_detail>-meins.
            IF sy-subrc EQ 0.
              <lfs_detail>-meins = ls_homoum-camposap.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDLOOP.

      MODIFY ENTITIES OF zmm_r_solped_header IN LOCAL MODE
        ENTITY header
          UPDATE FIELDS ( purchaserequisitiontype )
          WITH VALUE #( FOR ls_header IN lt_header  (
                             %tky      = ls_header-%tky
                             purchaserequisitiontype  = ls_header-purchaserequisitiontype ) ).

      zmm_solped_api_class=>get_instance( )->upload_detail( detail = lt_detail ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zmm_r_solped_header DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zmm_r_solped_header IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    zmm_solped_api_class=>get_instance( )->save(
      CHANGING
        reported = reported
    ).
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
