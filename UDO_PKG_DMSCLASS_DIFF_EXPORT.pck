create or replace package UDO_PKG_DMSCLASS_DIFF_EXPORT is

  type T_CODE_LIST is table of PKG_STD.TSTRING;
  type T_RN_LIST is table of PKG_STD.TREF;

  function GET_METADATA_DOMAINS return T_RN_LIST
    pipelined;
  function GET_CONDITIONS_DOMAINS return T_CODE_LIST
    pipelined;

  procedure EXPORT_CLASS_FULL
  (
    A_CLASS_RN       in number,
    A_FILEBUFF_IDENT in number
  );
  procedure SET_ENV
  (
    A_CLASS_RN       in number,
    A_FILEBUFF_IDENT in number
  );

  procedure EXPORT_METADATA;
end UDO_PKG_DMSCLASS_DIFF_EXPORT;
/
