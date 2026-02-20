CLASS zmm_solped_initialization DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: lv_message TYPE string.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS:
      fill_catval_cuenta EXPORTING message     TYPE string.

    METHODS:
      read_header_text EXPORTING message     TYPE string.
ENDCLASS.



CLASS ZMM_SOLPED_INITIALIZATION IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    fill_catval_cuenta(
      IMPORTING
        message = DATA(lv_message) ).

    out->write( lv_message ).
  ENDMETHOD.


  METHOD read_header_text.

  ENDMETHOD.


  METHOD fill_catval_cuenta.
    GET TIME STAMP FIELD DATA(lv_timestamp).

    DATA: lt_catval_cta TYPE STANDARD TABLE OF  ztb_a_catval_cta.

    lt_catval_cta = VALUE #(
        ( bklas = 'S001' sakto = '0063561010' )
        ( bklas = 'S002' sakto = '0063561010' )
        ( bklas = 'S003' sakto = '0063541010' )
        ( bklas = 'S004' sakto = '0063521010' )
        ( bklas = 'S005' sakto = '0063561010' )
        ( bklas = 'S006' sakto = '0063531010' )
        ( bklas = 'S007' sakto = '0063531010' )
        ( bklas = 'S008' sakto = '0063521010' )
        ( bklas = 'S009' sakto = '0063531010' )
        ( bklas = 'S010' sakto = '0063511010' )
        ( bklas = 'S011' sakto = '0063541010' )
        ( bklas = 'S012' sakto = '0063531010' )
        ( bklas = 'S013' sakto = '0063991120' )
        ( bklas = 'S014' sakto = '0063991120' )
        ( bklas = 'S015' sakto = '0064321010' )
        ( bklas = 'S016' sakto = '0063211010' )
        ( bklas = 'S017' sakto = '0063271010' )
        ( bklas = 'S018' sakto = '0063231010' )
        ( bklas = 'S019' sakto = '0063261010' )
        ( bklas = 'S020' sakto = '0063271010' )
        ( bklas = 'S021' sakto = '0063291080' )
        ( bklas = 'S022' sakto = '0063211020' )
        ( bklas = 'S023' sakto = '0063221010' )
        ( bklas = 'S024' sakto = '0063251010' )
        ( bklas = 'S025' sakto = '0063991090' )
        ( bklas = 'S026' sakto = '0063112040' )
        ( bklas = 'S027' sakto = '0063112030' )
        ( bklas = 'S028' sakto = '0065931010' )
        ( bklas = 'S029' sakto = '0063991130' )
        ( bklas = 'S030' sakto = '0063121010' )
        ( bklas = 'S031' sakto = '0063991090' )
        ( bklas = 'S032' sakto = '0063991090' )
        ( bklas = 'S033' sakto = '0063991090' )
        ( bklas = 'S034' sakto = '0063111010' )
        ( bklas = 'S035' sakto = '0063991160' )
        ( bklas = 'S036' sakto = '0063991090' )
        ( bklas = 'S037' sakto = '0064311010' )
        ( bklas = 'S038' sakto = '0063991130' )
        ( bklas = 'S039' sakto = '0063991160' )
        ( bklas = 'S040' sakto = '0063991130' )
        ( bklas = 'S041' sakto = '0063991130' )
        ( bklas = 'S042' sakto = '0065411010' )
        ( bklas = 'S043' sakto = '0065411020' )
        ( bklas = 'S044' sakto = '0065411020' )
        ( bklas = 'S045' sakto = '0065411010' )
        ( bklas = 'S046' sakto = '0063431030' )
        ( bklas = 'S047' sakto = '0063431040' )
        ( bklas = 'S048' sakto = '0063431030' )
        ( bklas = 'S049' sakto = '0063431030' )
        ( bklas = 'S050' sakto = '0063431030' )
        ( bklas = 'S051' sakto = '0063431030' )
        ( bklas = 'S052' sakto = '0063431020' )
        ( bklas = 'S053' sakto = '0063431020' )
        ( bklas = 'S054' sakto = '0063431030' )
        ( bklas = 'S055' sakto = '0063431030' )
        ( bklas = 'S056' sakto = '0063431030' )
        ( bklas = 'S057' sakto = '0063431030' )
        ( bklas = 'S058' sakto = '0063431030' )
        ( bklas = 'S059' sakto = '0063431010' )
        ( bklas = 'S060' sakto = '0063431020' )
        ( bklas = 'S061' sakto = '0063431020' )
        ( bklas = 'S062' sakto = '0063431030' )
        ( bklas = 'S063' sakto = '0063431030' )
        ( bklas = 'S064' sakto = '0063431030' )
        ( bklas = 'S065' sakto = '0063431050' )
        ( bklas = 'S066' sakto = '0063431050' )
        ( bklas = 'S067' sakto = '0063431030' )
        ( bklas = 'S068' sakto = '0063431030' )
        ( bklas = 'S069' sakto = '0063431030' )
        ( bklas = 'S070' sakto = '0063431030' )
        ( bklas = 'S071' sakto = '0063431030' )
        ( bklas = 'S072' sakto = '0063431030' )
        ( bklas = 'S073' sakto = '0063431070' )
        ( bklas = 'S074' sakto = '0063431020' )
        ( bklas = 'S075' sakto = '0063431030' )
        ( bklas = 'S076' sakto = '0063431020' )
        ( bklas = 'S077' sakto = '0063431030' )
        ( bklas = 'S078' sakto = '0063431070' )
        ( bklas = 'S079' sakto = '0063431010' )
        ( bklas = 'S080' sakto = '0063431010' )
        ( bklas = 'S081' sakto = '0063431030' )
        ( bklas = 'S082' sakto = '0063431010' )
        ( bklas = 'S083' sakto = '0063431030' )
        ( bklas = 'S084' sakto = '0063431030' )
        ( bklas = 'S085' sakto = '0063431030' )
        ( bklas = 'S086' sakto = '0063431030' )
        ( bklas = 'S087' sakto = '0063431030' )
        ( bklas = 'S088' sakto = '0063431090' )
        ( bklas = 'S089' sakto = '0063431070' )
        ( bklas = 'S090' sakto = '0063431020' )
        ( bklas = 'S091' sakto = '0063431020' )
        ( bklas = 'S092' sakto = '0063431020' )
        ( bklas = 'S093' sakto = '0063431020' )
        ( bklas = 'S094' sakto = '0063431020' )
        ( bklas = 'S095' sakto = '0063431020' )
        ( bklas = 'S096' sakto = '0063431020' )
        ( bklas = 'S097' sakto = '0063431040' )
        ( bklas = 'S098' sakto = '0063431030' )
        ( bklas = 'S099' sakto = '0063431040' )
        ( bklas = 'S100' sakto = '0063431090' )
        ( bklas = 'S101' sakto = '0063431030' )
        ( bklas = 'S102' sakto = '0063431030' )
        ( bklas = 'S103' sakto = '0063431030' )
        ( bklas = 'S104' sakto = '0063431030' )
        ( bklas = 'S105' sakto = '0063431030' )
        ( bklas = 'S106' sakto = '0063112020' )
        ( bklas = 'S107' sakto = '0063991090' )
        ( bklas = 'S108' sakto = '0063431030' )
        ( bklas = 'S109' sakto = '0063431030' )
        ( bklas = 'S110' sakto = '0063431040' )
        ( bklas = 'S111' sakto = '0063431030' )
        ( bklas = 'S112' sakto = '0063431030' )
        ( bklas = 'S113' sakto = '0063431020' )
        ( bklas = 'S114' sakto = '0063431020' )
        ( bklas = 'S115' sakto = '0063431070' )
        ( bklas = 'S116' sakto = '0063431010' )
        ( bklas = 'S117' sakto = '0063431030' )
        ( bklas = 'S118' sakto = '0063431010' )
        ( bklas = 'S119' sakto = '0065111060' )
        ( bklas = 'S120' sakto = '0065111060' )
        ( bklas = 'S121' sakto = '0065111060' )
        ( bklas = 'S122' sakto = '0065111040' )
        ( bklas = 'S123' sakto = '0065411020' )
        ( bklas = 'S124' sakto = '0062511060' )
        ( bklas = 'S125' sakto = '0062511040' )
        ( bklas = 'S126' sakto = '0062511040' )
        ( bklas = 'S127' sakto = '0063141010' )
        ( bklas = 'S128' sakto = '0063141010' )
        ( bklas = 'S129' sakto = '0063131010' )
        ( bklas = 'S130' sakto = '0063131010' )
        ( bklas = 'S131' sakto = '0065411020' )
        ( bklas = 'S132' sakto = '0063991090' )
        ( bklas = 'S133' sakto = '0063521010' )
        ( bklas = 'S134' sakto = '0063921010' )
        ( bklas = 'S135' sakto = '0063921010' )
        ( bklas = 'S136' sakto = '0063921010' )
        ( bklas = 'S137' sakto = '0063921010' )
        ( bklas = 'S138' sakto = '0063921010' )
        ( bklas = 'S139' sakto = '0063991090' )
        ( bklas = 'S140' sakto = '0063111030' )
        ( bklas = 'S141' sakto = '0063991180' )
        ( bklas = 'S142' sakto = '0063991090' )
        ( bklas = 'S143' sakto = '0063291080' )
        ( bklas = 'S144' sakto = '0063991090' )
        ( bklas = 'S145' sakto = '0063991090' )
        ( bklas = 'S146' sakto = '0063991090' )
        ( bklas = 'S147' sakto = '0063991090' )
        ( bklas = 'S148' sakto = '0063991090' )
        ( bklas = 'S149' sakto = '0063991090' )
        ( bklas = 'S150' sakto = '0063991090' )
        ( bklas = 'S151' sakto = '0063991090' )
        ( bklas = 'S152' sakto = '0063991100' )
        ( bklas = 'S153' sakto = '0063991100' )
        ( bklas = 'S154' sakto = '0063991090' )
        ( bklas = 'S155' sakto = '0063991130' )
        ( bklas = 'S156' sakto = '0063991130' )
        ( bklas = 'S157' sakto = '0063431090' )
        ( bklas = 'S158' sakto = '0063991130' )
        ( bklas = 'S159' sakto = '0063991090' )
        ( bklas = 'S160' sakto = '0063991130' )
        ( bklas = 'S161' sakto = '0063991130' )
        ( bklas = 'S162' sakto = '0063991130' )
        ( bklas = 'S163' sakto = '0063991130' )
        ( bklas = 'S164' sakto = '0063991130' )
        ( bklas = 'S165' sakto = '0063991130' )
        ( bklas = 'S166' sakto = '0063991090' )
        ( bklas = 'S167' sakto = '0063991010' )
        ( bklas = 'S168' sakto = '0063431090' )
        ( bklas = 'S169' sakto = '0063991110' )
        ( bklas = 'S170' sakto = '0063921010' )
        ( bklas = 'S171' sakto = '0063251010' )
        ( bklas = 'S172' sakto = '0063991020' )
        ( bklas = 'S173' sakto = '0063991090' )
        ( bklas = 'S174' sakto = '0063991090' )
        ( bklas = 'S175' sakto = '0063431010' )
        ( bklas = 'S176' sakto = '0063991140' )
        ( bklas = 'S177' sakto = '0063991090' )
        ( bklas = 'S178' sakto = '0063991090' )
        ( bklas = 'S179' sakto = '0063991090' )
        ( bklas = 'S180' sakto = '0063431020' )
        ( bklas = 'S181' sakto = '0063991090' )
        ( bklas = 'S182' sakto = '0063991150' )
        ( bklas = 'S183' sakto = '0063991090' )
        ( bklas = 'S184' sakto = '0063431040' )
        ( bklas = 'S185' sakto = '0063991090' )
        ( bklas = 'S186' sakto = '0063431020' )
        ( bklas = 'S187' sakto = '0063111020' )
        ( bklas = 'S188' sakto = '0063991090' )
        ( bklas = 'S189' sakto = '0063991090' )
        ( bklas = 'S190' sakto = '0063991130' )
        ( bklas = 'S191' sakto = '0063991020' )
        ( bklas = 'S192' sakto = '0063111030' )
        ( bklas = 'S193' sakto = '0063991090' )
        ( bklas = 'S194' sakto = '0063991120' )
        ( bklas = 'S195' sakto = '0063991040' )
        ( bklas = 'S196' sakto = '0062411010' )
        ( bklas = 'S197' sakto = '0063991090' )
        ( bklas = 'S198' sakto = '0065611070' )
        ( bklas = 'S199' sakto = '0063431010' )
        ( bklas = 'S200' sakto = '0063991130' )
        ( bklas = 'S201' sakto = '0063711010' )
        ( bklas = 'S202' sakto = '0063991090' )
        ( bklas = 'S203' sakto = '0063112010' )
        ( bklas = 'S204' sakto = '0063991090' )
        ( bklas = 'S205' sakto = '0063991150' )
        ( bklas = 'S206' sakto = '0065411020' )
        ( bklas = 'S207' sakto = '0065311010' )
        ( bklas = 'S208' sakto = '0063991080' )
        ( bklas = 'S209' sakto = '0063111020' )
        ( bklas = 'S210' sakto = '0063111020' )
        ( bklas = 'S211' sakto = '0063991090' )
        ( bklas = 'S212' sakto = '0063111020' )
    ).

    LOOP AT lt_catval_cta ASSIGNING FIELD-SYMBOL(<fs_catval_cta>).
      <fs_catval_cta>-local_created_by = sy-uname.
      <fs_catval_cta>-local_created_at = lv_timestamp.
      <fs_catval_cta>-local_last_changed_by = sy-uname.
      <fs_catval_cta>-local_last_changed_at = lv_timestamp.
      <fs_catval_cta>-last_changed_at = lv_timestamp.
    ENDLOOP.

*    DELETE FROM ztb_a_catval_cta.
    INSERT ztb_a_catval_cta FROM TABLE @lt_catval_cta.

    message = |Se cargaron los datos para tabla de Categoría valoración x Cta. Mayor.|.

  ENDMETHOD.
ENDCLASS.
