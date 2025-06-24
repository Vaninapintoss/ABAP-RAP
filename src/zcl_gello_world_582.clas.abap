CLASS zcl_gello_world_582 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gello_world_582 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( |Hello World! ({ cl_abap_context_info=>get_system_date( ) })| ).
  ENDMETHOD.

ENDCLASS.
