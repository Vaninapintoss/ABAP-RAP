CLASS zcl_ce_rap_agency_582 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_rap_query_provider.

    TYPES t_agency_range  TYPE RANGE OF z_travel_agency_es5-agencyid.
    TYPES t_business_data TYPE TABLE OF z_travel_agency_es5.

    METHODS get_agencies
      IMPORTING
        it_filter_cond     TYPE if_rap_query_filter=>tt_name_range_pairs OPTIONAL
        top                TYPE i OPTIONAL
        skip               TYPE i OPTIONAL
        is_data_requested  TYPE abap_bool OPTIONAL
        is_count_requested TYPE abap_bool OPTIONAL
      EXPORTING
        et_business_data   TYPE t_business_data
        count              TYPE int8
        ev_error_text      TYPE string
      RAISING
        /iwbep/cx_cp_remote
        /iwbep/cx_gateway
        cx_web_http_client_error
        cx_http_dest_provider_error.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ce_rap_agency_582 IMPLEMENTATION.
  METHOD get_agencies.
    DATA:
      lt_business_data TYPE TABLE OF zcl_rap_agency_012=>tys_z_travel_agency_es_5_type,
      lo_http_client   TYPE REF TO if_web_http_client,
      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request       TYPE REF TO /iwbep/if_cp_request_read_list,
      lo_response      TYPE REF TO /iwbep/if_cp_response_read_lst.


    DATA:
      lo_filter_factory   TYPE REF TO /iwbep/if_cp_filter_factory,
      lo_filter_node_1    TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_2    TYPE REF TO /iwbep/if_cp_filter_node,
      lo_filter_node_root TYPE REF TO /iwbep/if_cp_filter_node,
      lt_range_AGENCY_ID  TYPE RANGE OF z_travel_agency_es5-agencyid, "zcl_rap_agency_012=>tys_z_travel_agency_es_5_type-agency_id,
      lt_range_NAME       TYPE RANGE OF zcl_rap_agency_012=>tys_z_travel_agency_es_5_type-name.

    DATA service_consumption_name TYPE cl_web_odata_client_factory=>ty_service_definition_name.


    TRY.
        " Create http client
*DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                                             comm_scenario  = '<Comm Scenario>'
*                                             comm_system_id = '<Comm System Id>'
*                                             service_id     = '<Service Id>' ).
*lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).


*DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                         comm_scenario  = 'SAP_COM_0002'
*                         comm_system_id = 'ES5_SYSTEM'
*                         service_id     = 'ZAGENCYCDS_SRV' ).
*
*lo_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = lo_destination ).
*

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url = 'https://sapes5.sapdevcenter.com' ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = http_destination ).

        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING
             is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                 proxy_model_id      = 'ZSC_RAP_AGENCY_582'
                                                 proxy_model_version = '0001' )
            io_http_client             = lo_http_client
            iv_relative_service_root   = '/sap/opu/odata/sap/ZAGENCYCDS_SRV/' ).


        ASSERT lo_http_client IS BOUND.


        " Navigate to the resource and create a request for the read operation
        lo_request = lo_client_proxy->create_resource_for_entity_set( 'Z_TRAVEL_AGENCY_ES_5' )->create_request_for_read( ).

        " Create the filter tree
*        lo_filter_factory = lo_request->create_filter_factory( ).

*        lo_filter_node_1  = lo_filter_factory->create_by_range( iv_property_path     = 'AGENCY_ID'
*                                                                it_range             = lt_range_AGENCY_ID ).
*        lo_filter_node_2  = lo_filter_factory->create_by_range( iv_property_path     = 'NAME'
*                                                                it_range             = lt_range_NAME ).
*
*        lo_filter_node_root = lo_filter_node_1->and( lo_filter_node_2 ).
*        lo_request->set_filter( lo_filter_node_root ).


        lo_request->set_top( 50 )->set_skip( 0 ).

        " Execute the request and retrieve the business data
        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        et_business_data = lt_business_data.

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection
        ev_error_text = lx_remote->get_text( ).
      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        " Handle Exception
        ev_error_text = lx_gateway->get_text( ).
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        " Handle Exception
        RAISE SHORTDUMP lx_web_http_client_error.

    ENDTRY.


  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA business_data      TYPE t_business_data.
    DATA count              TYPE int8.
    DATA filter_conditions  TYPE if_rap_query_filter=>tt_name_range_pairs .
    DATA ranges_table       TYPE if_rap_query_filter=>tt_range_option .

    DATA error_text TYPE string.

*    ranges_table      = VALUE #( ( sign = 'I' option = 'GE' low = '099901' ) ).
*    filter_conditions = VALUE #( ( name = 'AGENCY_ID'  range = ranges_table ) ).

    TRY.
        get_agencies(
          EXPORTING
*            it_filter_cond     = filter_conditions
            top                = 3
            skip               = 0
            is_count_requested = abap_true
            is_data_requested  = abap_true
          IMPORTING
            et_business_data  = business_data
            count             = count
            ev_error_text    = error_text
          ) .

        IF error_text IS NOT INITIAL.
          out->write( |Ocurrió un error: { error_text }| ).
        ELSE.
          out->write( |Total number of records = { count }| ).
          out->write( business_data ).
        ENDIF.

      CATCH cx_root INTO DATA(exception).
        out->write( cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ) ).

    ENDTRY.
  ENDMETHOD.

  METHOD if_rap_query_provider~select.
    DATA business_data TYPE t_business_data.
    DATA(top)     = io_request->get_paging( )->get_page_size( ).
    DATA(skip)    = io_request->get_paging( )->get_offset( ).
    DATA(requested_fields)  = io_request->get_requested_elements( ).
    DATA(sort_order)    = io_request->get_sort_elements( ).
    DATA count TYPE int8.
    TRY.
        DATA(filter_condition) = io_request->get_filter( )->get_as_ranges( ).

        get_agencies(
                 EXPORTING
                   it_filter_cond        = filter_condition
                   top                = CONV i( top )
                   skip               = CONV i( skip )
                   is_data_requested  = io_request->is_data_requested( )
                   is_count_requested = io_request->is_total_numb_of_rec_requested(  )
                 IMPORTING
                   et_business_data  = business_data
                   count     = count
                 ) .

        IF io_request->is_total_numb_of_rec_requested(  ).
          io_response->set_total_number_of_records( count ).
        ENDIF.
        IF io_request->is_data_requested(  ).
          io_response->set_data( business_data ).
        ENDIF.

      CATCH cx_root INTO DATA(exception).
        DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_longtext( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
