CLASS zmm_solped_api_class DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*   Constructor
    CLASS-METHODS: get_instance RETURNING VALUE(ro_instance) TYPE REF TO zmm_solped_api_class.

    TYPES: tt_header_create TYPE TABLE FOR CREATE zmm_r_solped_header\\header.

    TYPES: tt_mapped_early   TYPE RESPONSE FOR MAPPED EARLY zmm_r_solped_header,
           tt_failed_early   TYPE RESPONSE FOR FAILED EARLY zmm_r_solped_header,
           tt_reported_early TYPE RESPONSE FOR REPORTED EARLY zmm_r_solped_header,
           tt_reported_late  TYPE RESPONSE FOR REPORTED LATE zmm_r_solped_header,
           tt_failed_late    TYPE RESPONSE FOR FAILED LATE zmm_r_solped_header.

    TYPES: tt_header_read        TYPE TABLE FOR READ IMPORT zmm_r_solped_header\\header,
           tt_header_read_result TYPE TABLE FOR READ RESULT zmm_r_solped_header\\header,
           tt_header_update      TYPE TABLE FOR UPDATE zmm_r_solped_header\\header,
           tt_header_delete      TYPE TABLE FOR DELETE zmm_r_solped_header\\header.

    TYPES: tt_detail_read        TYPE TABLE FOR READ IMPORT zmm_r_solped_header\\header\_detail,
           tt_detail_read_result TYPE TABLE FOR READ RESULT zmm_r_solped_header\\header\_detail,
           tt_detail_create      TYPE TABLE FOR CREATE zmm_r_solped_header\\header\_detail,
           tt_detail_update      TYPE TABLE FOR UPDATE zmm_r_solped_header\\detail,
           tt_detail_delete      TYPE TABLE FOR DELETE zmm_r_solped_header\\detail.

    TYPES: tt_headerid_rng TYPE RANGE OF sysuuid_x16,
           tt_detailid_rng TYPE RANGE OF zde_detail_id.

    TYPES: tt_keys_create_purreq TYPE TABLE FOR ACTION IMPORT zmm_r_solped_header\\header~actioncreatepurchasereq,
           ty_reported_pr        TYPE RESPONSE FOR REPORTED EARLY i_purchaserequisitiontp,
           ty_mapped_pr          TYPE RESPONSE FOR MAPPED EARLY i_purchaserequisitiontp,
           ty_failed_pr          TYPE RESPONSE FOR FAILED EARLY i_purchaserequisitiontp.

    TYPES: tt_detail TYPE STANDARD TABLE OF ztb_a_solped_det.

    METHODS:
      save CHANGING reported TYPE tt_reported_late.

    METHODS:
      earlynumbering_create_header IMPORTING entities TYPE tt_header_create
                                   CHANGING  mapped   TYPE tt_mapped_early
                                             failed   TYPE tt_failed_early
                                             reported TYPE tt_reported_early.

    METHODS:
      header_create IMPORTING entities TYPE tt_header_create
                    CHANGING  mapped   TYPE tt_mapped_early
                              failed   TYPE tt_failed_early
                              reported TYPE tt_reported_early.

    METHODS:
      header_read IMPORTING keys     TYPE tt_header_read
                  CHANGING  result   TYPE tt_header_read_result
                            failed   TYPE tt_failed_early
                            reported TYPE tt_reported_early.

    METHODS:
      header_update IMPORTING entities TYPE tt_header_update
                    CHANGING  mapped   TYPE tt_mapped_early
                              failed   TYPE tt_failed_early
                              reported TYPE tt_reported_early.

    METHODS:
      header_delete IMPORTING keys     TYPE tt_header_delete
                    CHANGING  mapped   TYPE tt_mapped_early
                              failed   TYPE tt_failed_early
                              reported TYPE tt_reported_early.
    METHODS:
      earlynumbering_cba_detail
        IMPORTING entities TYPE tt_detail_create
        CHANGING  mapped   TYPE tt_mapped_early
                  failed   TYPE tt_failed_early
                  reported TYPE tt_reported_early.

    METHODS:
      detail_create IMPORTING entities_cba TYPE tt_detail_create
                    CHANGING  mapped       TYPE tt_mapped_early
                              failed       TYPE tt_failed_early
                              reported     TYPE tt_reported_early.

    METHODS:
      detail_read IMPORTING keys_rba TYPE tt_detail_read
                  CHANGING  result   TYPE tt_detail_read_result
                            failed   TYPE tt_failed_early
                            reported TYPE tt_reported_early.

    METHODS:
      detail_update IMPORTING entities TYPE tt_detail_update
                    CHANGING  mapped   TYPE tt_mapped_early
                              failed   TYPE tt_failed_early
                              reported TYPE tt_reported_early.

    METHODS:
      detail_delete IMPORTING keys     TYPE tt_detail_delete
                    CHANGING  mapped   TYPE tt_mapped_early
                              failed   TYPE tt_failed_early
                              reported TYPE tt_reported_early.

    METHODS:
      upload_detail IMPORTING detail TYPE tt_detail.

    METHODS:
      create_purchase_requisition IMPORTING keys        TYPE tt_keys_create_purreq
                                  CHANGING  mapped_pr   TYPE ty_mapped_pr
                                            failed_pr   TYPE ty_failed_pr
                                            reported_pr TYPE ty_reported_pr.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA : mo_instance TYPE REF TO zmm_solped_api_class.

    CLASS-DATA : gt_header TYPE STANDARD TABLE OF ztb_a_solped_hea,
                 gt_detail TYPE STANDARD TABLE OF ztb_a_solped_det.

    CLASS-DATA: gt_header_del_rng TYPE tt_headerid_rng.

    CLASS-DATA: gt_detail_header_del_rng TYPE tt_headerid_rng,
                gt_detail_del_rng        TYPE tt_detailid_rng.

    CLASS-DATA: gs_mapped_pr TYPE ty_mapped_pr.
ENDCLASS.



CLASS ZMM_SOLPED_API_CLASS IMPLEMENTATION.


  METHOD create_purchase_requisition.
    DATA purchase_requisitions      TYPE TABLE FOR CREATE i_purchaserequisitiontp.
    DATA purchase_requisition_items TYPE TABLE FOR CREATE i_purchaserequisitiontp\_purchaserequisitionitem.
    DATA purchase_reqn_acct_assgmts TYPE TABLE FOR CREATE i_purchasereqnitemtp\_purchasereqnacctassgmt.
    DATA purchase_reqn_item_texts   TYPE TABLE FOR CREATE i_purchasereqnitemtp\_purchasereqnitemtext.

    DATA lt_header_update TYPE TABLE FOR UPDATE zmm_r_solped_header\\header.

    CLEAR gs_mapped_pr.

    READ ENTITIES OF zmm_r_solped_header IN LOCAL MODE
    ENTITY header
    ALL FIELDS
    WITH CORRESPONDING #( keys )
    RESULT DATA(lt_header).

    IF NOT lt_header IS INITIAL.
      READ ENTITIES OF zmm_r_solped_header IN LOCAL MODE
      ENTITY header BY \_detail
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_detail).

      SORT lt_detail BY detailid.

      LOOP AT lt_header INTO DATA(ls_header).

        "purchase requisition
        APPEND VALUE #( %cid                    = ls_header-headerid
                        purchaserequisitiontype = ls_header-purchaserequisitiontype
                        purreqndescription      = ''
                        ) TO purchase_requisitions.

        LOOP AT lt_detail INTO DATA(ls_detail) WHERE headerid EQ ls_header-headerid.
          "purchase requisition item
          APPEND VALUE #( %cid_ref = ls_detail-headerid
                          %target  = VALUE #( (
                                       %cid                           = ls_detail-detailid
                                       plant                          = ls_detail-plant
                                       storagelocation                = ls_detail-storagelocation
                                       accountassignmentcategory      = ls_detail-accountassignmentcategory
                                       purchasingdocumentitemcategory = ls_detail-purchasingdocumentitemcategory
*                                      PurchaseRequisitionItemText    =      "retrieved automatically from maintained MaterialInfo
                                       purchaserequisitionitemtext    = ls_detail-itemtext
                                       requestedquantity              = ls_detail-requestedquantity
                                       baseunit                       = ls_detail-baseunit
                                       material                       = ls_detail-material
*                                       materialgroup                  = 'A001'
                                       purchasinggroup                = ls_detail-purchasinggroup
                                       purchasingorganization         = ls_detail-purchasingorganization
*                                      DeliveryDate                   = CONV #( cl_abap_context_info=>get_system_date(  ) + 14 )
                                       deliverydate                   = ls_detail-deliverydate
                                       requisitionername              = ls_detail-requisitionername
                                       requirementtracking            = ls_detail-requirementtracking
                                       purchaserequisitionprice       = ls_detail-purchaserequisitionprice
                                       purreqnitemcurrency            = ls_detail-purreqnitemcurrency
                                       performanceperiodstartdate     = ls_detail-performanceperiodstartdate
                                       performanceperiodenddate       = ls_detail-performanceperiodenddate
                                     ) ) ) TO purchase_requisition_items.

          "purchase requisition item text
          APPEND VALUE #( %cid_ref = ls_detail-detailid
                          %target  = VALUE #( (
                                       %cid           = |Text-{ ls_detail-detailid }|
                                       textobjecttype = 'B01'
                                       language       = sy-langu
                                       plainlongtext  = ls_detail-itemtext
                                     ) ) ) TO purchase_reqn_item_texts.

          "purchase requisition item text
          APPEND VALUE #( %cid_ref = ls_detail-detailid
                          %target  = VALUE #( (
                                       %cid             = |Acct-{ ls_detail-detailid }|
                                       costcenter       = |{ ls_detail-costcenter ALPHA = IN }|
                                       glaccount        = ls_detail-glaccount
*                                       masterfixedasset = '10000017'
*                                       fixedasset = '0'
                                     ) ) ) TO purchase_reqn_acct_assgmts.
        ENDLOOP.

        MODIFY ENTITIES OF i_purchaserequisitiontp
          ENTITY purchaserequisition
            CREATE FIELDS ( purchaserequisitiontype
                            purreqndescription )
              WITH purchase_requisitions
            CREATE BY \_purchaserequisitionitem
              FIELDS ( plant
                       storagelocation
                       accountassignmentcategory
                       purchasingdocumentitemcategory
                       purchaserequisitionitemtext
                       requestedquantity
                       baseunit
                       material
*                       materialgroup
                       purchasinggroup
                       purchasingorganization
                       deliverydate
                       requisitionername
                       requirementtracking
                       purchaserequisitionprice
                       purreqnitemcurrency
                       performanceperiodstartdate
                       performanceperiodenddate
                     )
              WITH purchase_requisition_items
          ENTITY purchaserequisitionitem
            CREATE BY \_purchasereqnacctassgmt
              FIELDS ( costcenter
                       glaccount
*                       masterfixedasset
*                       fixedasset
                     )
              WITH purchase_reqn_acct_assgmts
            CREATE BY \_purchasereqnitemtext
              FIELDS ( textobjecttype
                       language
                       plainlongtext
                     )
              WITH purchase_reqn_item_texts
          REPORTED reported_pr
          MAPPED mapped_pr
          FAILED failed_pr.

        gs_mapped_pr = mapped_pr.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD detail_create.
    DATA: lt_detail_cre TYPE STANDARD TABLE OF ztb_a_solped_det.

    lt_detail_cre = VALUE #(
         FOR ls_entity_cba IN entities_cba
            FOR ls_detail_cba IN ls_entity_cba-%target
            LET
                ls_detail = CORRESPONDING ztb_a_solped_det(
                    ls_detail_cba MAPPING FROM ENTITY
                )
            IN
            (
                 ls_detail
            )
    ).

    mapped = VALUE #(
        detail = VALUE #(
            FOR i = 1 WHILE i <= lines( entities_cba )
            LET
            lt_detail = VALUE #( entities_cba[ i ]-%target OPTIONAL )
            IN
                FOR j = 1 WHILE j <= lines( lt_detail )
                LET
                ls_curr_detail = VALUE #( lt_detail[ j ] OPTIONAL )
                IN
                (
                    %cid = ls_curr_detail-%cid
                    %key = ls_curr_detail-%key
                    detailid = ls_curr_detail-detailid
                )
        )
    ).

    IF NOT lt_detail_cre IS INITIAL.
      APPEND LINES OF lt_detail_cre TO gt_detail.
    ENDIF.
  ENDMETHOD.


  METHOD detail_delete.
    gt_detail_header_del_rng = VALUE #(
        FOR i = 1 WHILE i <= lines( keys )
        LET
        ls_header_delete = VALUE #( keys[ i ] OPTIONAL )
        IN
        (
            sign = 'I'
            option = 'EQ'
            low = ls_header_delete-headerid
        )
    ).

    gt_detail_del_rng = VALUE #(
        FOR i = 1 WHILE i <= lines( keys )
        LET
        ls_header_delete = VALUE #( keys[ i ] OPTIONAL )
        IN
        (
            sign = 'I'
            option = 'EQ'
            low = ls_header_delete-detailid
        )
    ).
  ENDMETHOD.


  METHOD detail_read.
    SELECT * FROM ztb_a_solped_det FOR ALL ENTRIES IN @keys_rba
    WHERE header_id = @keys_rba-headerid
    INTO TABLE @DATA(lt_detail_data).

    result = VALUE #( FOR ls_detail IN lt_detail_data (
                headerid                        = ls_detail-header_id
                detailid                        = ls_detail-detail_id
                purchaserequisition             = ls_detail-banfn
                purchaserequisitionitem         = ls_detail-bnfpo
                purchasingorganization          = ls_detail-ekorg
                purchasinggroup                 = ls_detail-ekgrp
                requisitionername               = ls_detail-afnam
                plant                           = ls_detail-werks
                storagelocation                 = ls_detail-lgort
                accountassignmentcategory       = ls_detail-knttp
                purchasingdocumentitemcategory  = ls_detail-pstyp
                material                        = ls_detail-matnr
                requestedquantity               = ls_detail-menge
                baseunit                        = ls_detail-meins
                deliverydate                    = ls_detail-lfdat
                requirementtracking             = ls_detail-bednr
                itemtext                        = ls_detail-text
                purchaserequisitionprice        = ls_detail-preis
                purreqnitemcurrency             = ls_detail-waers
                glaccount                       = ls_detail-sakto
                costcenter                      = ls_detail-kostl
                performanceperiodstartdate      = ls_detail-startdate
                performanceperiodenddate        = ls_detail-enddate
                localcreatedby                  = ls_detail-local_created_by
                localcreatedat                  = ls_detail-local_created_at
                locallastchangedby              = ls_detail-local_last_changed_by
                locallastchangedat              = ls_detail-local_last_changed_at
                lastchangedat                   = ls_detail-last_changed_at
             ) ).
  ENDMETHOD.


  METHOD detail_update.
    DATA: lt_detail_upd TYPE STANDARD TABLE OF ztb_a_solped_det.

    DATA: lt_detail_update   TYPE STANDARD TABLE OF ztb_a_solped_det,
          lt_detail_update_x TYPE STANDARD TABLE OF zcs_solped_det.

    lt_detail_update = CORRESPONDING #( entities MAPPING FROM ENTITY ).
    lt_detail_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    IF NOT lt_detail_update IS INITIAL.
      SELECT * FROM ztb_a_solped_det
      FOR ALL ENTRIES IN @lt_detail_update
      WHERE header_id EQ @lt_detail_update-header_id
        AND detail_id EQ @lt_detail_update-detail_id
      INTO TABLE @DATA(lt_detail_old).
    ENDIF.

    lt_detail_upd = VALUE #(
        FOR i = 1  WHILE i <= lines( lt_detail_update )

        LET
            ls_control_flag = VALUE #( lt_detail_update_x[ i ] OPTIONAL )
            ls_detail_new = VALUE #( lt_detail_update[ i ] OPTIONAL )
            ls_detail_old = VALUE #( lt_detail_old[ header_id = ls_detail_new-header_id
                                                    detail_id = ls_detail_new-detail_id ] OPTIONAL )
        IN (
            header_id = ls_detail_new-header_id
            detail_id = ls_detail_new-detail_id
            banfn = COND #( WHEN NOT ls_control_flag-banfn IS INITIAL
                                 THEN ls_detail_new-banfn ELSE ls_detail_old-banfn )
            bnfpo = COND #( WHEN NOT ls_control_flag-bnfpo IS INITIAL
                                 THEN ls_detail_new-bnfpo ELSE ls_detail_old-bnfpo )
            ekorg = COND #( WHEN NOT ls_control_flag-ekorg IS INITIAL
                                 THEN ls_detail_new-ekorg ELSE ls_detail_old-ekorg )
            ekgrp = COND #( WHEN NOT ls_control_flag-ekgrp IS INITIAL
                                 THEN ls_detail_new-ekgrp ELSE ls_detail_old-ekgrp )
            afnam = COND #( WHEN NOT ls_control_flag-afnam IS INITIAL
                                 THEN ls_detail_new-afnam ELSE ls_detail_old-afnam )
            werks = COND #( WHEN NOT ls_control_flag-werks IS INITIAL
                                 THEN ls_detail_new-werks ELSE ls_detail_old-werks )
            lgort = COND #( WHEN NOT ls_control_flag-lgort IS INITIAL
                                 THEN ls_detail_new-lgort ELSE ls_detail_old-lgort )
            knttp = COND #( WHEN NOT ls_control_flag-knttp IS INITIAL
                                 THEN ls_detail_new-knttp ELSE ls_detail_old-knttp )
            pstyp = COND #( WHEN NOT ls_control_flag-pstyp IS INITIAL
                                 THEN ls_detail_new-pstyp ELSE ls_detail_old-pstyp )
            matnr = COND #( WHEN NOT ls_control_flag-matnr IS INITIAL
                                 THEN ls_detail_new-matnr ELSE ls_detail_old-matnr )
            menge = COND #( WHEN NOT ls_control_flag-menge IS INITIAL
                                 THEN ls_detail_new-menge ELSE ls_detail_old-menge )
            meins = COND #( WHEN NOT ls_control_flag-meins IS INITIAL
                                 THEN ls_detail_new-meins ELSE ls_detail_old-meins )
            lfdat = COND #( WHEN NOT ls_control_flag-lfdat IS INITIAL
                                 THEN ls_detail_new-lfdat ELSE ls_detail_old-lfdat )
            bednr = COND #( WHEN NOT ls_control_flag-bednr IS INITIAL
                                 THEN ls_detail_new-bednr ELSE ls_detail_old-bednr )
            text = COND #( WHEN NOT ls_control_flag-text IS INITIAL
                                 THEN ls_detail_new-text ELSE ls_detail_old-text )
            preis = COND #( WHEN NOT ls_control_flag-preis IS INITIAL
                                 THEN ls_detail_new-preis ELSE ls_detail_old-preis )
            waers = COND #( WHEN NOT ls_control_flag-waers IS INITIAL
                                 THEN ls_detail_new-waers ELSE ls_detail_old-waers )
            sakto = COND #( WHEN NOT ls_control_flag-sakto IS INITIAL
                                 THEN ls_detail_new-sakto ELSE ls_detail_old-sakto )
            kostl = COND #( WHEN NOT ls_control_flag-kostl IS INITIAL
                                 THEN ls_detail_new-kostl ELSE ls_detail_old-kostl )
            startdate = COND #( WHEN NOT ls_control_flag-startdate IS INITIAL
                                 THEN ls_detail_new-startdate ELSE ls_detail_old-startdate )
            enddate = COND #( WHEN NOT ls_control_flag-enddate IS INITIAL
                                 THEN ls_detail_new-enddate ELSE ls_detail_old-enddate )
            local_created_by = COND #( WHEN NOT ls_control_flag-local_created_by IS INITIAL
                                 THEN ls_detail_new-local_created_by ELSE ls_detail_old-local_created_by )
            local_created_at = COND #( WHEN NOT ls_control_flag-local_created_at IS INITIAL
                                 THEN ls_detail_new-local_created_at ELSE ls_detail_old-local_created_at )
            local_last_changed_by = COND #( WHEN NOT ls_control_flag-local_last_changed_by IS INITIAL
                                 THEN ls_detail_new-local_last_changed_by ELSE ls_detail_old-local_last_changed_by )
            local_last_changed_at = COND #( WHEN NOT ls_control_flag-local_last_changed_at IS INITIAL
                                 THEN ls_detail_new-local_last_changed_at ELSE ls_detail_old-local_last_changed_at )
            last_changed_at = COND #( WHEN NOT ls_control_flag-last_changed_at IS INITIAL
                                 THEN ls_detail_new-last_changed_at ELSE ls_detail_old-last_changed_at )
        )
    ).

    IF NOT lt_detail_upd IS INITIAL.
      APPEND LINES OF lt_detail_upd TO gt_detail.
    ENDIF.
  ENDMETHOD.


  METHOD earlynumbering_cba_detail.
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entitie>).
      LOOP AT <lfs_entitie>-%target ASSIGNING FIELD-SYMBOL(<lfs_detail>).
        SELECT MAX( detail_id ) FROM ztb_a_solped_det
        WHERE header_id EQ @<lfs_entitie>-headerid
        INTO @DATA(lv_detail_id_a).

        SELECT MAX( detailid ) FROM ztb_d_solped_det
        WHERE headerid EQ @<lfs_entitie>-headerid
        INTO @DATA(lv_detail_id_d).

        DATA(lv_detail_id) = COND #( WHEN lv_detail_id_a > lv_detail_id_d THEN lv_detail_id_a ELSE lv_detail_id_d ) + 1.

        mapped-detail = VALUE #( (
          %cid = <lfs_detail>-%cid
          %is_draft = <lfs_detail>-%is_draft
          %key = <lfs_detail>-%key
          detailid = lv_detail_id
        ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD earlynumbering_create_header.
    READ TABLE gt_header ASSIGNING FIELD-SYMBOL(<lfs_header>) INDEX 1.

    DATA(lo_uuid) = cl_uuid_factory=>create_system_uuid( ).

    TRY.
        DATA(lv_header_id) = lo_uuid->create_uuid_c32( ).
      CATCH cx_uuid_error.
        CLEAR lv_header_id.
    ENDTRY.

    IF <lfs_header> IS ASSIGNED.
      <lfs_header>-header_id = lv_header_id.
      UNASSIGN <lfs_header>.
    ENDIF.

    mapped-header = VALUE #(
        FOR ls_entitie IN entities WHERE ( headerid  IS INITIAL )
        (
            %cid =  ls_entitie-%cid
            %is_draft = ls_entitie-%is_draft
            headerid = lv_header_id
        )
    ).
  ENDMETHOD.


  METHOD get_instance.
    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                        THEN mo_instance
                                        ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD header_create.
    gt_header = CORRESPONDING #( entities MAPPING FROM ENTITY ).

    IF NOT gt_header IS INITIAL.
      mapped = VALUE #(
        header = VALUE #(
            FOR ls_entity IN entities (
              %cid = ls_entity-%cid
              %key = ls_entity-%key
              %is_draft = ls_entity-%is_draft ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD header_delete.
    gt_header_del_rng = VALUE #(
        FOR i = 1 WHILE i <= lines( keys )
        LET
        ls_header_delete = VALUE #( keys[ i ] OPTIONAL )
        IN
        (
            sign = 'I'
            option = 'EQ'
            low = ls_header_delete-headerid
        )
    ).
  ENDMETHOD.


  METHOD header_read.
    SELECT * FROM ztb_a_solped_hea FOR ALL ENTRIES IN @keys
    WHERE header_id = @keys-headerid
    INTO TABLE @DATA(lt_header_data).

    result = VALUE #( FOR ls_header IN lt_header_data (
                headerid                = ls_header-header_id
                attachment              = ls_header-attachment
                mimetype                = ls_header-mimetype
                filename                = ls_header-filename
                purchaserequisition     = ls_header-banfn
                purchaserequisitiontype = ls_header-bsart
                localcreatedby          = ls_header-local_created_by
                localcreatedat          = ls_header-local_created_at
                locallastchangedby      = ls_header-local_last_changed_by
                locallastchangedat      = ls_header-local_last_changed_at
                lastchangedat           = ls_header-last_changed_at
             ) ).


  ENDMETHOD.


  METHOD header_update.
    DATA: lt_header_update   TYPE STANDARD TABLE OF ztb_a_solped_hea,
          lt_header_update_x TYPE STANDARD TABLE OF zcs_solped_hea.

    lt_header_update = CORRESPONDING #( entities MAPPING FROM ENTITY ).
    lt_header_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    IF NOT lt_header_update IS INITIAL.
      SELECT * FROM ztb_a_solped_hea
      FOR ALL ENTRIES IN @lt_header_update
      WHERE header_id EQ @lt_header_update-header_id
      INTO TABLE @DATA(lt_header_old).
    ENDIF.

    gt_header = VALUE #(
        FOR i = 1  WHILE i <= lines( lt_header_update )

        LET
            ls_control_flag = VALUE #( lt_header_update_x[ i ] OPTIONAL )
            ls_header_new = VALUE #( lt_header_update[ i ] OPTIONAL )
            ls_header_old = VALUE #( lt_header_old[ header_id = ls_header_new-header_id ] OPTIONAL )
        IN (
            header_id = ls_header_new-header_id
            attachment = COND #( WHEN NOT ls_control_flag-attachment IS INITIAL
                                 THEN ls_header_new-attachment ELSE ls_header_old-attachment )
            mimetype = COND #( WHEN NOT ls_control_flag-mimetype IS INITIAL
                                 THEN ls_header_new-mimetype ELSE ls_header_old-mimetype )
            filename = COND #( WHEN NOT ls_control_flag-filename IS INITIAL
                                 THEN ls_header_new-filename ELSE ls_header_old-filename )
            banfn = COND #( WHEN NOT ls_control_flag-banfn IS INITIAL
                                 THEN ls_header_new-banfn ELSE ls_header_old-banfn )
            bsart = COND #( WHEN NOT ls_control_flag-bsart IS INITIAL
                                 THEN ls_header_new-bsart ELSE ls_header_old-bsart )
            local_created_by = COND #( WHEN NOT ls_control_flag-local_created_by IS INITIAL
                                 THEN ls_header_new-local_created_by ELSE ls_header_old-local_created_by )
            local_created_at = COND #( WHEN NOT ls_control_flag-local_created_at IS INITIAL
                                 THEN ls_header_new-local_created_at ELSE ls_header_old-local_created_at )
            local_last_changed_by = COND #( WHEN NOT ls_control_flag-local_last_changed_by IS INITIAL
                                 THEN ls_header_new-local_last_changed_by ELSE ls_header_old-local_last_changed_by )
            local_last_changed_at = COND #( WHEN NOT ls_control_flag-local_last_changed_at IS INITIAL
                                 THEN ls_header_new-local_last_changed_at ELSE ls_header_old-local_last_changed_at )
            last_changed_at = COND #( WHEN NOT ls_control_flag-last_changed_at IS INITIAL
                                 THEN ls_header_new-last_changed_at ELSE ls_header_old-last_changed_at )
        )
    ).

  ENDMETHOD.


  METHOD save.
*   Create / Update Header
    IF NOT gt_header IS INITIAL.
      MODIFY ztb_a_solped_hea FROM TABLE @gt_header.
    ENDIF.

*   Delete Header
    IF NOT gt_header_del_rng IS INITIAL.
      DELETE FROM ztb_a_solped_hea WHERE header_id IN @gt_header_del_rng.
      DELETE FROM ztb_a_solped_det WHERE header_id IN @gt_header_del_rng.
    ENDIF.

*   Create / Update Detail
    IF NOT gt_detail IS INITIAL.
      MODIFY ztb_a_solped_det FROM TABLE @gt_detail.
    ENDIF.

*   Delete Detail
    IF NOT gt_detail_header_del_rng IS INITIAL AND NOT gt_detail_del_rng IS INITIAL.
      DELETE FROM ztb_a_solped_det WHERE header_id IN @gt_detail_header_del_rng
                                     AND detail_id IN @gt_detail_del_rng.
    ENDIF.

*   Actualizar datos con Solped creada
    IF NOT gs_mapped_pr IS INITIAL.
      LOOP AT gs_mapped_pr-purchaserequisition  ASSIGNING FIELD-SYMBOL(<lfs_mapped_pr>).
        CONVERT KEY OF i_purchaserequisitiontp FROM <lfs_mapped_pr>-%pid TO DATA(ls_pr_key).
        DATA(lv_purchaserequisition) = ls_pr_key-purchaserequisition.
      ENDLOOP.

      IF NOT lv_purchaserequisition IS INITIAL.
        READ ENTITIES OF i_purchaserequisitiontp
        ENTITY purchaserequisition BY \_purchaserequisitionitem
        ALL FIELDS
        WITH VALUE #( ( purchaserequisition = lv_purchaserequisition ) )
        RESULT DATA(lt_purchaserequisitionitem).

        LOOP AT gt_header INTO DATA(ls_header).
          UPDATE ztb_a_solped_hea SET banfn = @lv_purchaserequisition
          WHERE header_id EQ @ls_header-header_id.

          IF NOT lt_purchaserequisitionitem IS INITIAL.
            LOOP AT lt_purchaserequisitionitem INTO DATA(ls_purchaserequisitionitem).
              UPDATE ztb_a_solped_det SET banfn = @ls_purchaserequisitionitem-purchaserequisition,
                                          bnfpo = @ls_purchaserequisitionitem-purchaserequisitionitem
              WHERE header_id EQ @ls_header-header_id AND
                    matnr EQ @ls_purchaserequisitionitem-material.
            ENDLOOP.
          ELSE.
            UPDATE ztb_a_solped_det SET banfn = @ls_purchaserequisitionitem-purchaserequisition
            WHERE header_id EQ @ls_header-header_id.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD upload_detail.
    gt_detail = detail.
  ENDMETHOD.
ENDCLASS.
