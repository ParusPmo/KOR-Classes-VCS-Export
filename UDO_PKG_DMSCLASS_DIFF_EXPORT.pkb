create or replace package body UDO_PKG_DMSCLASS_DIFF_EXPORT is

  WORKIN_CLASS      PKG_STD.TREF;
  FILE_BUFFER_IDENT PKG_STD.TREF;

  function NORMALIZE_FILENAME(S varchar2) return varchar2 deterministic is
    S2 PKG_STD.TSTRING;
  begin
    S2 := replace(S, ':', '_');
    S2 := replace(S2, '*', '_');
    S2 := replace(S2, '|', '_');
    S2 := replace(S2, '\', '_');
    S2 := replace(S2, '"', '_');
    S2 := replace(S2, '<', '_');
    S2 := replace(S2, '>', '_');
    S2 := replace(S2, '?', '_');
    S2 := replace(S2, '/', '_');
    return S2;
  
  end;

  procedure GET_RESOURCES_RU_UK
  (
    A_NRN         in number,
    A_TABLE_NAME  in varchar2,
    A_COLUMN_NAME in varchar2,
    A_TEXT_RU     out varchar2,
    A_TEXT_UK     out varchar2
  ) is
    cursor LC_RES is
      select RESOURCE_LANG,
             RESOURCE_TEXT
        from RESOURCES
       where TABLE_NAME = A_TABLE_NAME
         and RESOURCE_NAME = A_COLUMN_NAME
         and TABLE_ROW = A_NRN;
    L_RES LC_RES%rowtype;
  begin
    A_TEXT_RU := null;
    A_TEXT_UK := null;
    open LC_RES;
    loop
      fetch LC_RES
        into L_RES;
      exit when LC_RES%notfound;
      if L_RES.RESOURCE_LANG = 'RUSSIAN' then
        A_TEXT_RU := L_RES.RESOURCE_TEXT;
      elsif L_RES.RESOURCE_LANG = 'UKRAINIAN' then
        A_TEXT_UK := L_RES.RESOURCE_TEXT;
      end if;
    end loop;
    close LC_RES;
  end;

  function CONTEXT2S(N in number) return varchar2 is
  begin
    if N is null then
      return null;
    end if;
    case N
      when 0 then
        return 'Идентификатор записи';
      when 1 then
        return 'Идентификатор родительской записи';
      when 2 then
        return 'Идентификатор каталога';
      when 3 then
        return 'Идентификатор организации';
      when 4 then
        return 'Идентификатор версии';
      when 5 then
        return 'Код раздела';
      when 6 then
        return 'Код родительского раздела';
      when 7 then
        return 'Пользователь';
      when 8 then
        return 'NULL';
      when 9 then
        return 'Идентификатор отмеченных записей';
      when 10 then
        return 'Код мастер раздела';
      when 11 then
        return 'Идентификатор процесса';
      when 12 then
        return 'Идентификатор мастер записи';
      when 13 then
        return 'Метод вызова раздела';
    end case;
  end;

  procedure PUT_LINE
  (
    A_CONTENT in out clob,
    A_TEXT    in varchar2
  ) is
  begin
    if LENGTH(A_CONTENT) > 0 then
      A_CONTENT := A_CONTENT || CR;
    end if;
    A_CONTENT := A_CONTENT || A_TEXT;
  end;

  procedure PUT_CLOB
  (
    A_CONTENT in out clob,
    A_CLOB2   in clob
  ) is
  begin
    if LENGTH(A_CONTENT) > 0 then
      A_CONTENT := A_CONTENT || CR;
    end if;
    A_CONTENT := A_CONTENT || A_CLOB2;
  end;

  function GET_INDENT(A_LEVEL in number) return varchar2 is
    L_RESULT PKG_STD.TSTRING;
  begin
    for I in 1 .. A_LEVEL loop
      L_RESULT := L_RESULT || '    ';
    end loop;
    return L_RESULT;
  end;

  procedure PUT_TOML_TABLE
  (
    A_CONTENT    in out clob,
    A_TABLE_NAME in varchar2,
    A_IS_MULTY   in boolean default false,
    A_LEVEL      in number default 0
  ) is
    L_OPEN  PKG_STD.TSTRING;
    L_CLOSE PKG_STD.TSTRING;
  begin
    if A_IS_MULTY then
      L_OPEN  := '[["';
      L_CLOSE := '"]]';
    else
      L_OPEN  := '["';
      L_CLOSE := '"]';
    end if;
    PUT_LINE(A_CONTENT, GET_INDENT(A_LEVEL) || L_OPEN || A_TABLE_NAME || L_CLOSE);
  end;

  procedure PUT_TOML_PAIR
  (
    A_CONTENT in out clob,
    A_KEY     in varchar2,
    A_VALUE   in varchar2,
    A_LEVEL   in number default 0
  ) is
    L_LINE       PKG_STD.TSTRING;
    L_VALUE_QUOT varchar2(3) := '"';
  begin
    if INSTR(A_VALUE, CHR(10)) > 0 or INSTR(A_VALUE, '"') > 0 then
      L_VALUE_QUOT := '"""';
    end if;
    if A_VALUE is not null then
      L_LINE := GET_INDENT(A_LEVEL);
      L_LINE := L_LINE || '"' || A_KEY || '" = ' || L_VALUE_QUOT || A_VALUE || L_VALUE_QUOT;
      PUT_LINE(A_CONTENT, L_LINE);
    end if;
  end;

  procedure PUT_TOML_PAIR
  (
    A_CONTENT in out clob,
    A_KEY     in varchar2,
    A_VALUE   in number,
    A_LEVEL   in number default 0
  ) is
    L_LINE       PKG_STD.TSTRING;
    L_STR_VALUE  PKG_STD.TSTRING;
    L_FORMAT_STR PKG_STD.TSTRING;
  begin
    if A_VALUE is not null then
      if ABS(A_VALUE) < 1 and A_VALUE <> 0 then
        L_FORMAT_STR := 'fm0D9999999999999999999999999999999999999999999999999999999999999';
      else
        L_FORMAT_STR := 'tm';
      end if;
      L_STR_VALUE := TO_CHAR(A_VALUE, L_FORMAT_STR, 'nls_numeric_characters=''._''');
      L_LINE      := GET_INDENT(A_LEVEL);
      L_LINE      := L_LINE || '"' || A_KEY || '" = ' || L_STR_VALUE;
      PUT_LINE(A_CONTENT, L_LINE);
    end if;
  end;

  procedure PUT_TOML_PAIR
  (
    A_CONTENT in out clob,
    A_KEY     in varchar2,
    A_VALUE   in boolean,
    A_LEVEL   in number default 0
  ) is
    L_LINE      PKG_STD.TSTRING;
    L_STR_VALUE PKG_STD.TSTRING;
  begin
    if A_VALUE is not null then
      if A_VALUE then
        L_STR_VALUE := 'true';
      else
        L_STR_VALUE := 'false';
      end if;
      L_LINE := GET_INDENT(A_LEVEL);
      L_LINE := L_LINE || '"' || A_KEY || '" = ' || L_STR_VALUE;
      PUT_LINE(A_CONTENT, L_LINE);
    end if;
  end;

  procedure PUT_TOML_PAIR
  (
    A_CONTENT in out clob,
    A_KEY     in varchar2,
    A_VALUE   in date,
    A_LEVEL   in number default 0
  ) is
    L_LINE       PKG_STD.TSTRING;
    L_STR_VALUE  PKG_STD.TSTRING;
    L_FORMAT_STR PKG_STD.TSTRING;
  begin
    if A_VALUE is not null then
      if A_VALUE <> TRUNC(A_VALUE, 'DD') then
        L_FORMAT_STR := 'YYYY-MM-DD"T"HH24:MI:SS"Z"';
      else
        L_FORMAT_STR := 'YYYY-MM-DD';
      end if;
      L_STR_VALUE := TO_CHAR(A_VALUE, L_FORMAT_STR);
      L_LINE      := GET_INDENT(A_LEVEL);
      L_LINE      := L_LINE || '"' || A_KEY || '" = ' || L_STR_VALUE;
      PUT_LINE(A_CONTENT, L_LINE);
    end if;
  end;

  function CLOB_TO_BLOB
  (
    A_CDATA    in clob,
    A_CODEPAGE in varchar2
  ) return blob is
    L_BLOB_TMP     blob;
    L_DEST_OFFSET  integer := 1;
    L_SRC_OFFSET   integer := 1;
    L_LANG_CONTEXT integer := DBMS_LOB.DEFAULT_LANG_CTX;
    L_WARNING      integer := DBMS_LOB.WARN_INCONVERTIBLE_CHAR;
  begin
    DBMS_LOB.CREATETEMPORARY(L_BLOB_TMP, false);
    DBMS_LOB.CONVERTTOBLOB(DEST_LOB     => L_BLOB_TMP,
                           SRC_CLOB     => A_CDATA,
                           AMOUNT       => DBMS_LOB.LOBMAXSIZE,
                           DEST_OFFSET  => L_DEST_OFFSET,
                           SRC_OFFSET   => L_SRC_OFFSET,
                           BLOB_CSID    => NLS_CHARSET_ID(A_CODEPAGE),
                           LANG_CONTEXT => L_LANG_CONTEXT,
                           WARNING      => L_WARNING);
  
    return L_BLOB_TMP;
  end;

  procedure ADD_BUFF_BLOB
  (
    A_FILE_NAME in varchar2,
    A_DATA      blob
  ) is
  begin
    P_FILE_BUFFER_INSERT(NIDENT    => FILE_BUFFER_IDENT,
                         CFILENAME => NORMALIZE_FILENAME(A_FILE_NAME),
                         CDATA     => null,
                         BLOBDATA  => A_DATA);
  end;

  procedure ADD_BUFF_CLOB
  (
    A_FILE_NAME in varchar2,
    A_DATA      clob
  ) is
  begin
    P_FILE_BUFFER_INSERT(NIDENT    => FILE_BUFFER_IDENT,
                         CFILENAME => NORMALIZE_FILENAME(A_FILE_NAME),
                         CDATA     => A_DATA,
                         BLOBDATA  => null);
  end;

  procedure ADD_BUFF_XML
  (
    A_FILE_NAME in varchar2,
    A_DATA      clob
  ) is
    XML_HEAD constant clob := '<?xml version="1.0" encoding="windows-1251" standalone="yes"?>' || CR;
  begin
    P_FILE_BUFFER_INSERT(NIDENT    => FILE_BUFFER_IDENT,
                         CFILENAME => NORMALIZE_FILENAME(A_FILE_NAME),
                         CDATA     => XML_HEAD || XMLTYPE(A_DATA).EXTRACT('/*').GETCLOBVAL(),
                         BLOBDATA  => null);
    /*
    Oracle 11 
    SELECT XMLSERIALIZE(Document XMLTYPE('<html><body><p>Hallo, Welt.</p></body></html>') AS CLOB INDENT SIZE = 2) FROM dual;
    */
  end;

  function GET_CONDITIONS_DOMAINS return T_CODE_LIST
    pipelined is
    cursor LC_SETTINGS is
      select XMLTYPE(SETTINGS)
        from UNIT_SHOWMETHODS
       where PRN = WORKIN_CLASS
         and LENGTH(SETTINGS) > 0;
    cursor LC_DOMAINS(A_SETTINGS XMLTYPE) is
      select EXTRACTVALUE(value(T), '/Param/@Domain') as DOMAIN
        from table(XMLSEQUENCE(EXTRACT(A_SETTINGS, '/ShowMethod/Group/DataSource/Params/ConditionParams/Param'))) T;
    L_XSETTING XMLTYPE;
    L_DOMAIN   DMSDOMAINS.CODE%type;
  begin
    open LC_SETTINGS;
    loop
      fetch LC_SETTINGS
        into L_XSETTING;
      exit when LC_SETTINGS%notfound;
      open LC_DOMAINS(L_XSETTING);
      loop
        fetch LC_DOMAINS
          into L_DOMAIN;
        exit when LC_DOMAINS%notfound;
        pipe row(L_DOMAIN);
      end loop;
      close LC_DOMAINS;
    end loop;
    close LC_SETTINGS;
  end;

  function GET_METADATA_DOMAINS return T_RN_LIST
    pipelined is
    cursor LC_DOMAINS is
      select DOMAIN
        from DMSCLATTRS
       where PRN = WORKIN_CLASS
      union
      select DOMAIN
        from DMSCLACTIONSPRM T,
             UNITFUNC        F
       where F.PRN = WORKIN_CLASS
         and T.PRN = F.RN
      union
      select DOMAIN
        from DMSCLMETPARMS T,
             DMSCLMETHODS  M
       where M.PRN = WORKIN_CLASS
         and T.PRN = M.RN
      union
      select DOMAIN
        from DMSCLVIEWSPARAMS T,
             DMSCLVIEWS       V
       where V.PRN = WORKIN_CLASS
         and T.PRN = V.RN;
    L_DOMAIN PKG_STD.TREF;
  begin
    open LC_DOMAINS;
    loop
      fetch LC_DOMAINS
        into L_DOMAIN;
      exit when LC_DOMAINS%notfound;
      pipe row(L_DOMAIN);
    end loop;
    close LC_DOMAINS;
  end;

  function GET_DOMAINS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_DOMAINS is
      select D.RN,
             D.CODE,
             DT.DATATYPE_TEXT,
             DT.DATATYPE_SUBTEXT,
             D.DATA_LENGTH,
             D.DATA_PRECISION,
             D.DATA_SCALE,
             D.DEFAULT_STR,
             D.DEFAULT_NUM,
             D.DEFAULT_DATE,
             D.ENUMERATED,
             D.PADDING
        from DMSDOMAINS  D,
             V_DATATYPES DT
       where D.RN in (select *
                        from table(GET_METADATA_DOMAINS)
                      union
                      select RN
                        from DMSDOMAINS DD
                       where DD.CODE in (select *
                                           from table(GET_CONDITIONS_DOMAINS)))
         and D.DATA_TYPE = DT.DATATYPE_NUMB
         and D.DATA_SUBTYPE = DT.DATATYPE_SUBNUMB
       order by CODE;
    L_DOMAIN  LC_DOMAINS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
  
    procedure PUT_ENUM_VALS(P_PRN number) is
      cursor LC_ENUM_VALS is
        select RN,
               POSITION,
               VALUE_STR,
               VALUE_NUM,
               VALUE_DATE
          from DMSENUMVALUES T
         where PRN = P_PRN
         order by POSITION;
      L_ENUM_VAL LC_ENUM_VALS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Домен.Перечисляемые значения', false, A_LEVEL + 2);
      open LC_ENUM_VALS;
      loop
        fetch LC_ENUM_VALS
          into L_ENUM_VAL;
        exit when LC_ENUM_VALS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Перечисляемое значение', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', TO_NUMBER(trim(L_ENUM_VAL.POSITION)), A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ENUM_VAL.VALUE_STR, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ENUM_VAL.VALUE_NUM, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ENUM_VAL.VALUE_DATE, A_LEVEL + 4);
        GET_RESOURCES_RU_UK(L_ENUM_VAL.RN, 'DMSENUMVALUES', 'NAME', L_TEXT_RU, L_TEXT_UK);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 4);
      end loop;
      close LC_ENUM_VALS;
    end;
  begin
    PUT_LINE(L_SECTION, '# Все используемые классом домены');
    PUT_TOML_TABLE(L_SECTION, 'Используемые домены', false, A_LEVEL);
    open LC_DOMAINS;
    loop
      fetch LC_DOMAINS
        into L_DOMAIN;
      exit when LC_DOMAINS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Домен', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Мнемокод', L_DOMAIN.CODE, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_DOMAIN.RN, 'DMSDOMAINS', 'NAME', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Тип данных', L_DOMAIN.DATATYPE_TEXT, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Подтип данных', L_DOMAIN.DATATYPE_SUBTEXT, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Размер строки', L_DOMAIN.DATA_LENGTH, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Точность данных', L_DOMAIN.DATA_PRECISION, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Дробность данных', L_DOMAIN.DATA_SCALE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Значение по умолчанию', L_DOMAIN.DEFAULT_STR, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Значение по умолчанию', L_DOMAIN.DEFAULT_NUM, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Значение по умолчанию', L_DOMAIN.DEFAULT_DATE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Выравнивать по длине', INT2BOOL(L_DOMAIN.PADDING), A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Имеет перечисляемые значения',
                    INT2BOOL(L_DOMAIN.ENUMERATED),
                    A_LEVEL + 2);
      if L_DOMAIN.ENUMERATED = 1 then
        PUT_ENUM_VALS(L_DOMAIN.RN);
      end if;
    end loop;
    close LC_DOMAINS;
    return L_SECTION;
  end;

  function GET_CLASS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_CLASS is
      select CL.*,
             (select I.CODE
                from SYSIMAGES I
               where I.RN = CL.SYSIMAGE) as SSYSIMAGE,
             UA.CODE as SDOCFORM
        from UNITLIST  CL,
             UAMODULES UA
       where CL.RN = WORKIN_CLASS
         and CL.DOCFORM = UA.RN(+);
    L_CLASS LC_CLASS%rowtype;
    cursor LC_TABLE(A_TABLENAME varchar2) is
      select TL.*
        from TABLELIST TL
       where TL.TABLENAME = A_TABLENAME;
    L_TABLE   LC_TABLE%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
  begin
    open LC_CLASS;
    fetch LC_CLASS
      into L_CLASS;
    close LC_CLASS;
    if L_CLASS.UNITCODE is null then
      return L_SECTION;
    end if;
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Метаданные класса');
    PUT_LINE(L_SECTION, '');
    PUT_TOML_TABLE(L_SECTION, 'Класс', false, A_LEVEL);
    PUT_TOML_PAIR(L_SECTION, 'Код', L_CLASS.UNITCODE, A_LEVEL + 1);
    GET_RESOURCES_RU_UK(WORKIN_CLASS, 'UNITLIST', 'UNITNAME', L_TEXT_RU, L_TEXT_UK);
    PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Абстрактный', INT2BOOL(L_CLASS.ABSTRACT), A_LEVEL + 1);
    if L_CLASS.TABLE_NAME is not null then
      open LC_TABLE(L_CLASS.TABLE_NAME);
      fetch LC_TABLE
        into L_TABLE;
      close LC_TABLE;
      PUT_TOML_TABLE(L_SECTION, 'Таблица', false, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Имя', L_TABLE.TABLENAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_TABLE.RN, 'TABLELIST', 'TABLENOTE', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_TABLE.TEMPFLAG
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип информации', 'Постоянная', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип информации', 'Временная', A_LEVEL + 2);
      end case;
      case L_TABLE.TECHNOLOGY
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Технология производства', 'Стандарт', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION,
                        'Технология производства',
                        'Конструктор',
                        A_LEVEL + 2);
      end case;
    end if;
    --    PUT_TOML_PAIR(L_SECTION, 'Таблица', L_CLASS.TABLE_NAME, A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Буферный', INT2BOOL(L_CLASS.SIGN_BUFFER), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Ведомый', INT2BOOL(L_CLASS.SIGN_DRIVEN), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Ведущий раздел', L_CLASS.HOSTCODE, A_LEVEL + 1);
    case L_CLASS.SIGN_SHARE * (L_CLASS.SIGN_ACCREG + 1)
      when 0 then
        PUT_TOML_PAIR(L_SECTION, 'Деление', 'Нет деления', A_LEVEL + 1);
      when 1 then
        PUT_TOML_PAIR(L_SECTION, 'Деление', 'По версиям', A_LEVEL + 1);
      when 2 then
        PUT_TOML_PAIR(L_SECTION, 'Деление', 'По организациям', A_LEVEL + 1);
    end case;
    PUT_TOML_PAIR(L_SECTION, 'Юридические лица', INT2BOOL(L_CLASS.SIGN_JURPERS), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Иерархия', INT2BOOL(L_CLASS.HIERARCHICAL), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Каталоги', INT2BOOL(L_CLASS.SIGN_HIER), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Свойства документов', INT2BOOL(L_CLASS.USE_DOCPROPS), A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION,
                  'Присоединенные документы',
                  INT2BOOL(L_CLASS.USE_FILELINKS),
                  A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION,
                  'Процедура считывания значений атрибутов',
                  L_CLASS.GET_PROCEDURE,
                  A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Форма раздела', L_CLASS.SDOCFORM, A_LEVEL + 1);
    PUT_TOML_PAIR(L_SECTION, 'Пиктограмма', L_CLASS.SSYSIMAGE, A_LEVEL + 1);
    return L_SECTION;
  end;

  function GET_ATTRIBUTES_SECTION(A_LEVEL in number default 0) return clob is
    cursor CL_ATTRS is
      select CA.*,
             DM.CODE            as SDOMAIN,
             CL.CONSTRAINT_NAME as SREF_LINK,
             CAR.COLUMN_NAME    as SREF_ATTRIBUTE
        from DMSCLATTRS CA,
             DMSDOMAINS DM,
             DMSCLLINKS CL,
             DMSCLATTRS CAR
       where CA.PRN = WORKIN_CLASS
         and CA.DOMAIN = DM.RN
         and CA.REF_LINK = CL.RN(+)
         and CA.REF_ATTRIBUTE = CAR.RN(+)
       order by CA.POSITION;
    C_ATTR    CL_ATTRS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Атрибуты класса');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Атрибуты', false, A_LEVEL);
    open CL_ATTRS;
    loop
      fetch CL_ATTRS
        into C_ATTR;
      exit when CL_ATTRS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Класс.Атрибут', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Имя', C_ATTR.COLUMN_NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(C_ATTR.RN, 'DMSCLATTRS', 'CAPTION', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Позиция', C_ATTR.POSITION, A_LEVEL + 2);
      case C_ATTR.KIND
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Физический', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Логический', A_LEVEL + 2);
        when 2 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Получен по связи', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Домен', C_ATTR.SDOMAIN, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Связь', C_ATTR.SREF_LINK, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Атрибут связи', C_ATTR.SREF_ATTRIBUTE, A_LEVEL + 2);
    end loop;
    close CL_ATTRS;
    return L_SECTION;
  end;

  function GET_CONSTRAINTS_SECTION(A_LEVEL in number default 0) return clob is
    cursor CL_CLCONSTRS is
      select T.*,
             MES.CODE       as MES_CODE,
             MES.TECHNOLOGY as MES_TECHNOLOGY,
             MES.KIND       as MES_KIND
        from DMSCLCONSTRS T,
             DMSMESSAGES  MES
       where T.PRN = WORKIN_CLASS
         and T.MESSAGE = MES.RN(+)
       order by T.CONSTRAINT_TYPE,
                T.CONSTRAINT_NAME;
    L_CLCONSTR CL_CLCONSTRS%rowtype;
    L_SECTION  clob;
    L_TEXT_RU  PKG_STD.TSTRING;
    L_TEXT_UK  PKG_STD.TSTRING;
    procedure PUT_CONSATTR(A_CONS in number) is
      cursor LC_CLCONATTRS is
        select T.POSITION,
               TR1.COLUMN_NAME
          from DMSCLCONATTRS T,
               DMSCLATTRS    TR1
         where T.PRN = A_CONS
           and T.ATTRIBUTE = TR1.RN
         order by T.POSITION;
      L_CLCONATTR LC_CLCONATTRS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Ограничения.Атрибуты', false, A_LEVEL + 2);
      open LC_CLCONATTRS;
      loop
        fetch LC_CLCONATTRS
          into L_CLCONATTR;
        exit when LC_CLCONATTRS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Атрибут', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', L_CLCONATTR.POSITION, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут', L_CLCONATTR.COLUMN_NAME, A_LEVEL + 4);
      end loop;
      close LC_CLCONATTRS;
    end;
    procedure PUT_MESSAGE is
    begin
      PUT_TOML_TABLE(L_SECTION,
                     'Ограничение.Сообщение при нарушениии',
                     false,
                     A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Мнемокод', L_CLCONSTR.MES_CODE, A_LEVEL + 3);
      case L_CLCONSTR.MES_TECHNOLOGY
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Технология производства', 'Стандарт', A_LEVEL + 3);
        when 1 then
          PUT_TOML_PAIR(L_SECTION,
                        'Технология производства',
                        'Конструктор',
                        A_LEVEL + 3);
      end case;
      case L_CLCONSTR.MES_KIND
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Сообщение ограничения', A_LEVEL + 3);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Сообщение исключения', A_LEVEL + 3);
      end case;
      GET_RESOURCES_RU_UK(L_CLCONSTR.MESSAGE, 'DMSMESSAGES', 'TEXT', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Текст (RU)', L_TEXT_RU, A_LEVEL + 3);
      PUT_TOML_PAIR(L_SECTION, 'Текст (UK)', L_TEXT_UK, A_LEVEL + 3);
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Ограничения класса');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Ограничения', false, A_LEVEL);
    open CL_CLCONSTRS;
    loop
      fetch CL_CLCONSTRS
        into L_CLCONSTR;
      exit when CL_CLCONSTRS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Ограничение', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Имя', L_CLCONSTR.CONSTRAINT_NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_CLCONSTR.RN, 'DMSCLCONSTRS', 'CONSTRAINT_NOTE', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_CLCONSTR.CONSTRAINT_TYPE
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Уникальность', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Первичный ключ', A_LEVEL + 2);
        when 2 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Проверка', A_LEVEL + 2);
        when 5 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Обязательность', A_LEVEL + 2);
        when 6 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Неизменяемость', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION,
                    'Использовать для разрешения ссылок',
                    INT2BOOL(L_CLCONSTR.LINKS_SIGN),
                    A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Текст ограничения', L_CLCONSTR.CONSTRAINT_TEXT, A_LEVEL + 2);
      if L_CLCONSTR.MESSAGE is not null then
        PUT_MESSAGE;
      end if;
      PUT_CONSATTR(L_CLCONSTR.RN);
    end loop;
    close CL_CLCONSTRS;
    return L_SECTION;
  end;

  function GET_LINKS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_LINKS is
      select T.RN,
             T.CONSTRAINT_NAME,
             US.UNITCODE,
             ST.CODE            as SSTEREOTYPE,
             T.FOREIGN_KEY,
             CC.CONSTRAINT_NAME as SSRC_CONSTRAINT,
             T.RULE,
             L.CONSTRAINT_NAME  as SMASTER_LINK,
             M1.CODE            as SMESSAGE1,
             M2.CODE            as SMESSAGE2,
             LA.COLUMN_NAME     as SLEVEL_ATTR,
             PA.COLUMN_NAME     as SPATH_ATTR
        from DMSCLLINKS   T,
             UNITLIST     US,
             DMSLSTYPES   ST,
             DMSCLCONSTRS CC,
             DMSCLLINKS   L,
             DMSMESSAGES  M1,
             DMSMESSAGES  M2,
             DMSCLATTRS   LA,
             DMSCLATTRS   PA
       where T.DESTINATION = WORKIN_CLASS
         and T.SOURCE = US.RN
         and T.STEREOTYPE = ST.RN(+)
         and T.SRC_CONSTRAINT = CC.RN(+)
         and T.MASTER_LINK = L.RN(+)
         and T.MESSAGE1 = M1.RN(+)
         and T.MESSAGE2 = M2.RN(+)
         and T.LEVEL_ATTR = LA.RN(+)
         and T.PATH_ATTR = PA.RN(+)
       order by T.CONSTRAINT_NAME;
    L_LINK    LC_LINKS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
    procedure PUT_ATTRS(A_LINK in number) is
      cursor LC_LINKATTRS is
        select T.POSITION,
               TR1.COLUMN_NAME as SSOURCE,
               TR2.COLUMN_NAME as SDESTINATION
          from DMSCLLINKATTRS T,
               DMSCLATTRS     TR1,
               DMSCLATTRS     TR2
         where T.PRN = A_LINK
           and T.SOURCE = TR1.RN
           and T.DESTINATION = TR2.RN
         order by T.POSITION;
      L_LINKATTR LC_LINKATTRS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Связи.Атрибуты', false, A_LEVEL + 2);
      open LC_LINKATTRS;
      loop
        fetch LC_LINKATTRS
          into L_LINKATTR;
        exit when LC_LINKATTRS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Атрибут', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', L_LINKATTR.POSITION, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION,
                      'Атрибут класса-приемника',
                      L_LINKATTR.SDESTINATION,
                      A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут класса-источника', L_LINKATTR.SSOURCE, A_LEVEL + 4);
      end loop;
      close LC_LINKATTRS;
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Связи класса');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Связи', false, A_LEVEL);
    open LC_LINKS;
    loop
      fetch LC_LINKS
        into L_LINK;
      exit when LC_LINKS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Связь', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Код', L_LINK.CONSTRAINT_NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_LINK.RN, 'DMSCLLINKS', 'CONSTRAINT_NOTE', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Класс-источник', L_LINK.UNITCODE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Стереотип', L_LINK.SSTEREOTYPE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Физическая связь', INT2BOOL(L_LINK.FOREIGN_KEY), A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Ограничение класса-источника',
                    L_LINK.SSRC_CONSTRAINT,
                    A_LEVEL + 2);
      case L_LINK.RULE
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Правило', 'Нет правил', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Правило', 'Каскадное удаление', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Мастер-связь', L_LINK.SMASTER_LINK, A_LEVEL + 2);
      if (L_LINK.SMESSAGE1 is not null) or (L_LINK.SMESSAGE2 is not null) then
        PUT_TOML_TABLE(L_SECTION, 'Связь.Сообщение при нарушениии', false, A_LEVEL + 2);
        PUT_TOML_PAIR(L_SECTION, 'Со стороны источника', L_LINK.SMESSAGE1, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Со стороны приемника', L_LINK.SMESSAGE2, A_LEVEL + 3);
      end if;
      if (L_LINK.SLEVEL_ATTR is not null) or (L_LINK.SPATH_ATTR is not null) then
        PUT_TOML_TABLE(L_SECTION, 'Связь.Иерархия', false, A_LEVEL + 2);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут уровня', L_LINK.SLEVEL_ATTR, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут полного имени', L_LINK.SPATH_ATTR, A_LEVEL + 3);
      end if;
      PUT_ATTRS(L_LINK.RN);
    end loop;
    close LC_LINKS;
    return L_SECTION;
  end;

  function GET_VIEWS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_VIEWS is
      select T.RN,
             T.VIEW_NAME,
             T.CUSTOM_QUERY,
             T.ACCESSIBILITY,
             T.QUERY_SQL
        from DMSCLVIEWS T
       where T.PRN = WORKIN_CLASS
       order by T.VIEW_NAME;
    L_VIEW    LC_VIEWS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
    procedure PUT_ATTRS(A_VIEW number) is
      cursor LC_VIEWATTRS is
        select A.POSITION,
               A.COLUMN_NAME as SATTR,
               T.COLUMN_NAME
          from DMSCLVIEWSATTRS T,
               DMSCLATTRS      A
         where T.PRN = A_VIEW
           and T.ATTR = A.RN
         order by A.POSITION;
      L_VIEWATTR LC_VIEWATTRS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Представления.Атрибуты', false, A_LEVEL + 2);
      open LC_VIEWATTRS;
      loop
        fetch LC_VIEWATTRS
          into L_VIEWATTR;
        exit when LC_VIEWATTRS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Атрибут', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут класса', L_VIEWATTR.SATTR, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Имя колонки', L_VIEWATTR.COLUMN_NAME, A_LEVEL + 4);
      end loop;
      close LC_VIEWATTRS;
    end;
    procedure PUT_PARAMS(A_VIEW number) is
      cursor LC_VIEWPARAMS is
        select T.PARAM_NAME,
               D.CODE as SDOMAIN
          from DMSCLVIEWSPARAMS T,
               DMSDOMAINS       D
         where T.PRN = A_VIEW
           and T.DOMAIN = D.RN
         order by T.PARAM_NAME;
      L_VIEWPARAM LC_VIEWPARAMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Представления.Параметры', false, A_LEVEL + 2);
      open LC_VIEWPARAMS;
      loop
        fetch LC_VIEWPARAMS
          into L_VIEWPARAM;
        exit when LC_VIEWPARAMS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Параметр', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Наименование параметра', L_VIEWPARAM.PARAM_NAME, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Домен', L_VIEWPARAM.SDOMAIN, A_LEVEL + 4);
      end loop;
      close LC_VIEWPARAMS;
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Представления класса');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Представления', false, A_LEVEL);
    open LC_VIEWS;
    loop
      fetch LC_VIEWS
        into L_VIEW;
      exit when LC_VIEWS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Представление', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Имя', L_VIEW.VIEW_NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_VIEW.RN, 'DMSCLVIEWS', 'VIEW_NOTE', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_VIEW.CUSTOM_QUERY
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Представление', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип', 'Запрос', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Вызывается с клиента', INT2BOOL(L_VIEW.ACCESSIBILITY), A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Текст запроса', L_VIEW.QUERY_SQL, A_LEVEL + 2);
      PUT_ATTRS(L_VIEW.RN);
      PUT_PARAMS(L_VIEW.RN);
    end loop;
    close LC_VIEWS;
    return L_SECTION;
  end;

  function GET_METHODS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_METHODS is
      select T.RN,
             T.CODE,
             T.METHOD_TYPE,
             T.ACCESSIBILITY,
             T.PACKAGE,
             T.NAME,
             (select D.CODE
                from DMSCLMETPARMS P,
                     DMSDOMAINS    D
               where P.DOMAIN = D.RN
                 and P.PRN = T.RN
                 and P.NAME = 'RESULT'
                 and T.METHOD_TYPE = 1) as SRESULT_DOMAIN
        from DMSCLMETHODS T
       where T.PRN = WORKIN_CLASS
       order by T.CODE;
    L_METHOD  LC_METHODS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
    procedure PUT_PARAMS(A_METHOD in number) is
      cursor LC_METPARAMS is
        select T.RN,
               T.POSITION,
               T.NAME,
               T.INOUT,
               D.CODE         as SDOMAIN,
               T.LINK_TYPE,
               A.COLUMN_NAME,
               T.DEF_NUMBER,
               T.CONTEXT,
               T.DEF_STRING,
               T.DEF_DATE,
               F.CODE         as LINKED_FUNCTION,
               T.ACTION_PARAM,
               T.MANDATORY
          from DMSCLMETPARMS T,
               DMSDOMAINS    D,
               DMSCLATTRS    A,
               DMSCLMETHODS  F
         where T.PRN = A_METHOD
           and T.DOMAIN = D.RN
           and T.LINK_ATTR = A.RN(+)
           and T.LINKED_FUNCTION = F.RN(+)
         order by T.POSITION;
      L_METPARAM LC_METPARAMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Методы.Параметры', false, A_LEVEL + 2);
      open LC_METPARAMS;
      loop
        fetch LC_METPARAMS
          into L_METPARAM;
        exit when LC_METPARAMS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Параметр', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Имя', L_METPARAM.NAME, A_LEVEL + 4);
        GET_RESOURCES_RU_UK(L_METPARAM.RN, 'DMSCLMETPARMS', 'NOTE', L_TEXT_RU, L_TEXT_UK);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', L_METPARAM.POSITION, A_LEVEL + 4);
        case L_METPARAM.INOUT
          when 0 then
            PUT_TOML_PAIR(L_SECTION, 'Тип', 'Входной/выходной (in/out)', A_LEVEL + 4);
          when 1 then
            PUT_TOML_PAIR(L_SECTION, 'Тип', 'Входной (in)', A_LEVEL + 4);
          when 2 then
            PUT_TOML_PAIR(L_SECTION, 'Тип', 'Выходной (out)', A_LEVEL + 4);
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Домен', L_METPARAM.SDOMAIN, A_LEVEL + 4);
        case L_METPARAM.LINK_TYPE
          when 0 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Нет', A_LEVEL + 4);
          when 1 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Атрибут', A_LEVEL + 4);
          when 2 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Контекст', A_LEVEL + 4);
          when 3 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Значение', A_LEVEL + 4);
          when 4 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Результат функции', A_LEVEL + 4);
          when 5 then
            PUT_TOML_PAIR(L_SECTION, 'Тип привязки', 'Параметр действия', A_LEVEL + 4);
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Атрибут', L_METPARAM.COLUMN_NAME, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_METPARAM.DEF_NUMBER, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Контекст', CONTEXT2S(L_METPARAM.CONTEXT), A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_METPARAM.DEF_STRING, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_METPARAM.DEF_DATE, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Функция', L_METPARAM.LINKED_FUNCTION, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Параметр действия', L_METPARAM.ACTION_PARAM, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION,
                      'Обязательный для заполнения',
                      INT2BOOL(L_METPARAM.MANDATORY),
                      A_LEVEL + 4);
      end loop;
      close LC_METPARAMS;
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Методы класса');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Методы', false, A_LEVEL);
    open LC_METHODS;
    loop
      fetch LC_METHODS
        into L_METHOD;
      exit when LC_METHODS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Метод', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Мнемокод', L_METHOD.CODE, A_LEVEL + 2);
      case L_METHOD.METHOD_TYPE
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип метода', 'Процедура', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип метода', 'Функция', A_LEVEL + 2);
      end case;
      case L_METHOD.ACCESSIBILITY
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Доступность', 'Базовый', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Доступность', 'Клиентский', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Пакет', L_METHOD.PACKAGE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Процедура/функция', L_METHOD.NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_METHOD.RN, 'DMSCLMETHODS', 'NOTE', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_METHOD.RN, 'DMSCLMETHODS', 'COMMENT', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Примечание (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Примечание (UK)', L_TEXT_UK, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Домен результата функции', L_METHOD.SRESULT_DOMAIN, A_LEVEL + 2);
      PUT_PARAMS(L_METHOD.RN);
    end loop;
    close LC_METHODS;
    return L_SECTION;
  end;

  procedure PUT_FORM_SECTION
  (
    A_SECTION   in out clob,
    A_LEVEL     in number,
    A_FORM_RN   in varchar2,
    A_TAB_PREF  in varchar2,
    A_FILE_PREF in varchar2
  ) is
    cursor LC_FORM is
      select T.RN,
             T.FORM_CLASS,
             T.FORM_NAME,
             T.EVENTS_LANGUAGE,
             F_USERFORMS_GET_UAMODULE(T.FORM_UAMODULE) as SFORM_UAMODULE,
             T.FORM_LANGUAGE,
             T.FORM_ACTIVE,
             T.LINK_APPS,
             T.LINK_PRIVS,
             T.FORM_DATA,
             T.FORM_EVENTS,
             T.FORM_DATA_EXT,
             T.FORM_EVENTS_EXT
        from USERFORMS T
       where T.RN = A_FORM_RN
       order by T.FORM_CLASS,
                T.FORM_LANGUAGE;
    L_FORM LC_FORM%rowtype;
    cursor LC_FORM_APPS is
      select FLA.APPCODE
        from USERFORMLNKAPPS FLA
       where FLA.PRN = A_FORM_RN
       order by FLA.APPCODE;
    L_FORM_APP LC_FORM_APPS%rowtype;
  begin
    open LC_FORM;
    fetch LC_FORM
      into L_FORM;
    close LC_FORM;
  
    PUT_LINE(A_SECTION, '');
    PUT_TOML_TABLE(A_SECTION, 'Форма', true, A_LEVEL);
    PUT_TOML_PAIR(A_SECTION, 'Имя', L_FORM.FORM_CLASS, A_LEVEL + 1);
    PUT_TOML_PAIR(A_SECTION, 'Наименование', L_FORM.FORM_NAME, A_LEVEL + 1);
    case L_FORM.EVENTS_LANGUAGE
      when 0 then
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'VBScript', A_LEVEL + 1);
      when 1 then
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'JScript', A_LEVEL + 1);
      when 2 then
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'DelphiScript', A_LEVEL + 1);
      when 3 then
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'PerlScript', A_LEVEL + 1);
      when 4 then
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'PythonScript', A_LEVEL + 1);
      else
        PUT_TOML_PAIR(A_SECTION, 'Тип скрипта', 'Не задано', A_LEVEL + 1);
    end case;
    PUT_TOML_PAIR(A_SECTION,
                  'Пользовательское приложение (форма)',
                  L_FORM.SFORM_UAMODULE,
                  A_LEVEL + 1);
    case L_FORM.FORM_LANGUAGE
      when 'RUSSIAN' then
        PUT_TOML_PAIR(A_SECTION, 'Национальный язык формы', 'Русский', A_LEVEL + 1);
      when 'UKRAINIAN' then
        PUT_TOML_PAIR(A_SECTION, 'Национальный язык формы', 'Украинский', A_LEVEL + 1);
      else
        PUT_TOML_PAIR(A_SECTION, 'Национальный язык формы', 'Не задано', A_LEVEL + 1);
    end case;
    PUT_TOML_PAIR(A_SECTION,
                  'Доступна для использования',
                  INT2BOOL(L_FORM.FORM_ACTIVE),
                  A_LEVEL + 1);
    PUT_TOML_PAIR(A_SECTION,
                  'Учитывать связи с приложениями',
                  INT2BOOL(L_FORM.LINK_APPS),
                  A_LEVEL + 1);
    PUT_TOML_PAIR(A_SECTION,
                  'Учитывать назначение пользователям, ролям',
                  INT2BOOL(L_FORM.LINK_PRIVS),
                  A_LEVEL + 1);
    if L_FORM.LINK_APPS = 1 then
      PUT_TOML_TABLE(A_SECTION, A_TAB_PREF || '.Приложения', false, A_LEVEL + 1);
      open LC_FORM_APPS;
      loop
        fetch LC_FORM_APPS
          into L_FORM_APP;
        exit when LC_FORM_APPS%notfound;
        PUT_LINE(A_SECTION, '');
        PUT_TOML_TABLE(A_SECTION, 'Приложение', true, A_LEVEL + 2);
        PUT_TOML_PAIR(A_SECTION, 'Код', L_FORM_APP.APPCODE, A_LEVEL + 3);
      
      end loop;
      close LC_FORM_APPS;
    end if;
    if L_FORM.FORM_DATA is not null and LENGTH(L_FORM.FORM_DATA) > 0 then
      ADD_BUFF_XML(A_FILE_PREF || '_' || L_FORM.FORM_CLASS || '_' || L_FORM.FORM_LANGUAGE || '.xml', L_FORM.FORM_DATA);
    end if;
    if L_FORM.FORM_EVENTS is not null and LENGTH(L_FORM.FORM_EVENTS) > 0 then
      ADD_BUFF_CLOB(A_FILE_PREF || '_Events_' || L_FORM.FORM_CLASS || '_' || L_FORM.FORM_LANGUAGE || '.xml',
                    L_FORM.FORM_EVENTS);
    end if;
    if L_FORM.FORM_DATA_EXT is not null and LENGTH(L_FORM.FORM_DATA_EXT) > 0 then
      ADD_BUFF_XML(A_FILE_PREF || '_Cond_' || L_FORM.FORM_CLASS || '_' || L_FORM.FORM_LANGUAGE || '.xml',
                   L_FORM.FORM_DATA_EXT);
    end if;
    if L_FORM.FORM_EVENTS_EXT is not null and LENGTH(L_FORM.FORM_EVENTS_EXT) > 0 then
      ADD_BUFF_CLOB(A_FILE_PREF || '_Cond_Events_' || L_FORM.FORM_CLASS || '_' || L_FORM.FORM_LANGUAGE || '.xml',
                    L_FORM.FORM_EVENTS_EXT);
    end if;
  end;

  function GET_SHOWMETODS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_SHOWMETHODS is
      select SM.RN,
             SM.METHOD_CODE,
             SM.TECHNOLOGY,
             (select I.CODE
                from SYSIMAGES I
               where I.RN = SM.SYSIMAGE) as SSYSIMAGE,
             SM.COND_TYPE,
             SM.USEFORVIEW,
             SM.USEFORLINKS,
             SM.USEFORDICT,
             SM.SETTINGS
        from UNIT_SHOWMETHODS SM
       where SM.PRN = WORKIN_CLASS
       order by SM.TECHNOLOGY,
                SM.METHOD_CODE;
    L_SHOWMETHOD        LC_SHOWMETHODS%rowtype;
    L_SECTION           clob;
    L_TEXT_RU           PKG_STD.TSTRING;
    L_TEXT_UK           PKG_STD.TSTRING;
    L_SETTINGS_FILENAME PKG_STD.TSTRING;
    procedure PUT_PARAMS(A_METHOD in number) is
      cursor LC_UNITPARAMS is
        select MP.RN,
               CA.COLUMN_NAME,
               MP.IN_CODE,
               MP.OUT_CODE,
               MP.DATA_TYPE,
               MP.DIRECT_SQL,
               MP.BACK_SQL
          from UNITPARAMS MP,
               DMSCLATTRS CA
         where MP.PARENT_METHOD = A_METHOD
           and MP.ATTRIBUTE = CA.RN(+)
         order by MP.TECHNOLOGY,
                  MP.IN_CODE,
                  MP.OUT_CODE;
      L_UNITPARAM LC_UNITPARAMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Методы вызова.Параметры', false, A_LEVEL + 2);
      open LC_UNITPARAMS;
      loop
        fetch LC_UNITPARAMS
          into L_UNITPARAM;
        exit when LC_UNITPARAMS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Параметр', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут класса', L_UNITPARAM.COLUMN_NAME, A_LEVEL + 4);
        GET_RESOURCES_RU_UK(L_UNITPARAM.RN, 'UNITPARAMS', 'PARAMNAME', L_TEXT_RU, L_TEXT_UK);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Имя входного параметра', L_UNITPARAM.IN_CODE, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Имя выходного параметра', L_UNITPARAM.OUT_CODE, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION,
                      'Настройка источника данных',
                      L_UNITPARAM.OUT_CODE,
                      A_LEVEL + 4);
        case L_UNITPARAM.DATA_TYPE
          when 0 then
            PUT_TOML_PAIR(L_SECTION, 'Тип данных', 'Строка', A_LEVEL + 4);
          when 1 then
            PUT_TOML_PAIR(L_SECTION, 'Тип данных', 'Дата', A_LEVEL + 4);
          when 2 then
            PUT_TOML_PAIR(L_SECTION, 'Тип данных', 'Число', A_LEVEL + 4);
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Прямой запрос', L_UNITPARAM.DIRECT_SQL, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Обратный запрос', L_UNITPARAM.BACK_SQL, A_LEVEL + 4);
      end loop;
      close LC_UNITPARAMS;
    end;
    procedure PUT_FORMS(A_METHOD in number) is
      cursor LC_UNITFORMS is
        select T.RN
          from USERFORMS T
         where SHOW_METHOD = A_METHOD
           and FORM_KIND = 5
           and FORM_ID = 0
         order by T.FORM_CLASS,
                  T.FORM_LANGUAGE;
      L_UNITFORM LC_UNITFORMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Методы вызова.Формы', false, A_LEVEL + 2);
      open LC_UNITFORMS;
      loop
        fetch LC_UNITFORMS
          into L_UNITFORM;
        exit when LC_UNITFORMS%notfound;
        PUT_FORM_SECTION(L_SECTION,
                         A_LEVEL + 3,
                         L_UNITFORM.RN,
                         'Класс.Методы вызова.Формы',
                         'Form_ShowMetod_' || L_SHOWMETHOD.METHOD_CODE);
      end loop;
      close LC_UNITFORMS;
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Методы вызова');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Методы вызова', false, A_LEVEL);
    open LC_SHOWMETHODS;
    loop
      fetch LC_SHOWMETHODS
        into L_SHOWMETHOD;
      exit when LC_SHOWMETHODS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Метод вызова', true, A_LEVEL + 1);
      PUT_TOML_PAIR(L_SECTION, 'Мнемокод', L_SHOWMETHOD.METHOD_CODE, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_SHOWMETHOD.RN, 'UNIT_SHOWMETHODS', 'METHOD_NAME', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_SHOWMETHOD.TECHNOLOGY
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Технология производства', 'Стандарт', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION,
                        'Технология производства',
                        'Конструктор',
                        A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Пиктограмма', L_SHOWMETHOD.SSYSIMAGE, A_LEVEL + 2);
      case L_SHOWMETHOD.COND_TYPE
        when 0 then
          PUT_TOML_PAIR(L_SECTION, 'Тип условий отбора', 'Клиент', A_LEVEL + 2);
        when 1 then
          PUT_TOML_PAIR(L_SECTION, 'Тип условий отбора', 'Сервер', A_LEVEL + 2);
      end case;
      PUT_TOML_PAIR(L_SECTION,
                    'Использовать для отображения по умолчанию',
                    INT2BOOL(L_SHOWMETHOD.USEFORVIEW),
                    A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Использовать для отображения через связи документов',
                    INT2BOOL(L_SHOWMETHOD.USEFORLINKS),
                    A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Использовать для отображения в качестве словаря',
                    INT2BOOL(L_SHOWMETHOD.USEFORDICT),
                    A_LEVEL + 2);
      if L_SHOWMETHOD.SETTINGS is not null and LENGTH(L_SHOWMETHOD.SETTINGS) > 0 then
        L_SETTINGS_FILENAME := 'ShowMethodSettings_' || L_SHOWMETHOD.METHOD_CODE || '.xml';
        PUT_TOML_PAIR(L_SECTION, 'Настройка', L_SETTINGS_FILENAME, A_LEVEL + 2);
        ADD_BUFF_XML(L_SETTINGS_FILENAME, L_SHOWMETHOD.SETTINGS);
      end if;
      PUT_PARAMS(L_SHOWMETHOD.RN);
      PUT_FORMS(L_SHOWMETHOD.RN);
    end loop;
    close LC_SHOWMETHODS;
    return L_SECTION;
  end;

  function GET_ACTIONS_SECTION(A_LEVEL in number default 0) return clob is
    cursor LC_ACTIONS is
      select UF.RN,
             UF.STANDARD,
             UF.DETAILCODE,
             UF.CODE,
             UF.TECHNOLOGY,
             UF.NUMB,
             M.CODE as SMETHOD,
             (select I.CODE
                from SYSIMAGES I
               where I.RN = UF.SYSIMAGE) as SSYSIMAGE,
             UF.PROCESS_MODE,
             UF.TRANSACT_MODE,
             UF.REFRESH_MODE,
             UF.SHOW_DIALOG,
             UF.ONLY_CUSTOM_MODE,
             UF.OVERRIDE,
             UF.UNCOND_ACCESS
        from UNITFUNC     UF,
             DMSCLMETHODS M
       where UF.PRN = WORKIN_CLASS
         and UF.METHOD = M.RN(+)
       order by UF.NUMB;
    L_ACTION  LC_ACTIONS%rowtype;
    L_SECTION clob;
    L_TEXT_RU PKG_STD.TSTRING;
    L_TEXT_UK PKG_STD.TSTRING;
    L_STRVAL  PKG_STD.TSTRING;
    procedure PUT_FORMS
    (
      A_ACTION      in number,
      A_ACTION_CODE in varchar2
    ) is
      cursor LC_ACTFORMS is
        select T.RN
          from USERFORMS T
         where FORM_KIND = 3
           and FORM_ID = A_ACTION
         order by T.FORM_CLASS,
                  T.FORM_LANGUAGE;
      L_ACTFORM LC_ACTFORMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Действия.Формы', false, A_LEVEL + 2);
      open LC_ACTFORMS;
      loop
        fetch LC_ACTFORMS
          into L_ACTFORM;
        exit when LC_ACTFORMS%notfound;
        PUT_FORM_SECTION(L_SECTION,
                         A_LEVEL + 3,
                         L_ACTFORM.RN,
                         'Класс.Действия.Формы',
                         'Form_Action_' || A_ACTION_CODE);
      end loop;
      close LC_ACTFORMS;
    end;
    procedure PUT_PARAMS(A_ACTION in number) is
      cursor LC_ACTPRMS is
        select T.RN,
               T.NAME,
               T.POSITION,
               D.CODE        as SDOMAIN,
               T.LINK_TYPE,
               A.COLUMN_NAME as SLINK_ATTR,
               T.CONTEXT,
               T.DEF_NUMBER,
               T.DEF_STRING,
               T.DEF_DATE,
               F.CODE        as SLINKED_FUNCTION,
               T.SM_PARAM
          from DMSCLACTIONSPRM T,
               DMSDOMAINS      D,
               DMSCLATTRS      A,
               DMSCLMETHODS    F
         where T.PRN = A_ACTION
           and T.DOMAIN = D.RN
           and T.LINK_ATTR = A.RN(+)
           and T.LINKED_FUNCTION = F.RN(+)
         order by T.POSITION;
      L_ACTPRM LC_ACTPRMS%rowtype;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Действия.Параметры', false, A_LEVEL + 2);
      open LC_ACTPRMS;
      loop
        fetch LC_ACTPRMS
          into L_ACTPRM;
        exit when LC_ACTPRMS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Параметр', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Имя', L_ACTPRM.NAME, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', L_ACTPRM.POSITION, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Домен', L_ACTPRM.SDOMAIN, A_LEVEL + 4);
        case L_ACTPRM.LINK_TYPE
          when 0 then
            L_STRVAL := 'Нет';
          when 1 then
            L_STRVAL := 'Атрибут';
          when 2 then
            L_STRVAL := 'Контекст';
          when 3 then
            L_STRVAL := 'Значение';
          when 4 then
            L_STRVAL := 'Результат функции';
          when 5 then
            L_STRVAL := 'Параметр метода вызова';
          else
            L_STRVAL := null;
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Тип привязки', L_STRVAL, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Атрибут', L_ACTPRM.SLINK_ATTR, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Контекст', CONTEXT2S(L_ACTPRM.CONTEXT), A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ACTPRM.DEF_NUMBER, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ACTPRM.DEF_STRING, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Значение', L_ACTPRM.DEF_DATE, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Функция', L_ACTPRM.SLINKED_FUNCTION, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Параметр метода вызова', L_ACTPRM.SM_PARAM, A_LEVEL + 4);
      end loop;
      close LC_ACTPRMS;
    end;
    procedure PUT_METHODS
    (
      A_ACTION      in number,
      A_ACTION_CODE in varchar2
    ) is
      cursor LC_ACTMETHS is
        select T.RN,
               F.CODE
          from DMSCLACTIONSMTH T,
               DMSCLMETHODS    F
         where T.PRN = A_ACTION
           and T.METHOD = F.RN
         order by F.CODE;
      L_ACTMETH LC_ACTMETHS%rowtype;
      procedure PUT_METHFORMS
      (
        A_METHOD    in number,
        A_METH_CODE in varchar2
      ) is
        cursor LC_METHFORMS is
          select T.RN
            from USERFORMS T
           where FORM_KIND = 3
             and FORM_ID = A_METHOD
           order by T.FORM_CLASS,
                    T.FORM_LANGUAGE;
        L_METHFORM LC_METHFORMS%rowtype;
      begin
        PUT_TOML_TABLE(L_SECTION, 'Класс.Действия.Методы.Формы', false, A_LEVEL + 4);
        open LC_METHFORMS;
        loop
          fetch LC_METHFORMS
            into L_METHFORM;
          exit when LC_METHFORMS%notfound;
          PUT_FORM_SECTION(L_SECTION,
                           A_LEVEL + 5,
                           L_METHFORM.RN,
                           'Класс.Действия.Методы.Формы',
                           'Form_Action_' || A_ACTION_CODE || '_Method_' || A_METH_CODE);
        end loop;
        close LC_METHFORMS;
      end;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Действия.Методы', false, A_LEVEL + 2);
      open LC_ACTMETHS;
      loop
        fetch LC_ACTMETHS
          into L_ACTMETH;
        exit when LC_ACTMETHS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Метод', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Метод', L_ACTMETH.CODE, A_LEVEL + 4);
        PUT_METHFORMS(L_ACTMETH.RN, L_ACTMETH.CODE);
      end loop;
      close LC_ACTMETHS;
    end;
    procedure PUT_STEPS
    (
      A_ACTION      in number,
      A_ACTION_CODE in varchar2
    ) is
      cursor LC_STEPS is
        select T.RN,
               T.POSITION,
               T.STPTYPE,
               P.NAME         as SEXEC_PARAM,
               SM.UNITCODE    as SSHOWUNIT,
               SM.METHOD_CODE as SSHOWMETHOD,
               T.SHOWPARAMS,
               T.SHOWKIND,
               R.CODE         as SUSERREPORT,
               UAM.CODE       as SUAMODULE,
               UAMA.CODE      as SUAMODULE_ACTION
          from DMSCLACTIONSSTP  T,
               DMSCLACTIONSPRM  P,
               UNIT_SHOWMETHODS SM,
               USERREPORTS      R,
               UAMODULES        UAM,
               UAMACTIONS       UAMA
         where T.PRN = A_ACTION
           and T.EXEC_PARAM = P.RN(+)
           and T.SHOWMETHOD = SM.RN(+)
           and T.USERREPORT = R.RN(+)
           and T.UAMODULE = UAM.RN(+)
           and T.UAMODULE_ACTION = UAMA.RN(+)
         order by T.POSITION;
      L_STEP              LC_STEPS%rowtype;
      L_SETTINGS_FILENAME PKG_STD.TSTRING;
    begin
      PUT_TOML_TABLE(L_SECTION, 'Класс.Действия.Шаги', false, A_LEVEL + 2);
      open LC_STEPS;
      loop
        fetch LC_STEPS
          into L_STEP;
        exit when LC_STEPS%notfound;
        PUT_LINE(L_SECTION, '');
        PUT_TOML_TABLE(L_SECTION, 'Шаг', true, A_LEVEL + 3);
        PUT_TOML_PAIR(L_SECTION, 'Позиция', L_STEP.POSITION, A_LEVEL + 4);
        case L_STEP.STPTYPE
          when 0 then
            L_STRVAL := 'Выполнить действие';
          when 1 then
            L_STRVAL := 'Открыть раздел';
          when 2 then
            L_STRVAL := 'Пользовательский отчет';
          when 3 then
            L_STRVAL := 'Пользовательское приложение';
          else
            L_STRVAL := null;
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Тип', L_STRVAL, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Параметр действия', L_STEP.SEXEC_PARAM, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Раздел', L_STEP.SSHOWUNIT, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Метод вызова', L_STEP.SSHOWMETHOD, A_LEVEL + 4);
        if L_STEP.SHOWPARAMS is not null and LENGTH(L_STEP.SHOWPARAMS) > 0 then
          L_SETTINGS_FILENAME := 'Action_' || A_ACTION_CODE || '_StepShowParams_' || L_STEP.POSITION || '.xml';
          PUT_TOML_PAIR(L_SECTION, 'Параметры метода вызова', L_SETTINGS_FILENAME, A_LEVEL + 4);
          ADD_BUFF_XML(L_SETTINGS_FILENAME, L_STEP.SHOWPARAMS);
        end if;
        case L_STEP.SHOWKIND
          when 0 then
            L_STRVAL := 'Обычный';
          when 1 then
            L_STRVAL := 'Модальный';
          when 2 then
            L_STRVAL := 'Как словарь';
          else
            L_STRVAL := null;
        end case;
        PUT_TOML_PAIR(L_SECTION, 'Режим вызова', L_STRVAL, A_LEVEL + 4);
        PUT_TOML_PAIR(L_SECTION, 'Пользовательский отчет', L_STEP.SUSERREPORT, A_LEVEL + 4);
        if L_STEP.STPTYPE = 3 then
          PUT_TOML_TABLE(L_SECTION, 'Пользовательское приложение', false, A_LEVEL + 4);
          PUT_TOML_PAIR(L_SECTION, 'Модуль', L_STEP.SUAMODULE, A_LEVEL + 5);
          PUT_TOML_PAIR(L_SECTION, 'Действие', L_STEP.SUAMODULE_ACTION, A_LEVEL + 5);
        end if;
      end loop;
      close LC_STEPS;
    end;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Действия');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Действия', false, A_LEVEL);
    open LC_ACTIONS;
    loop
      fetch LC_ACTIONS
        into L_ACTION;
      exit when LC_ACTIONS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Действие', true, A_LEVEL + 1);
      case L_ACTION.STANDARD
        when 0 then
          L_STRVAL := 'Нестандартное';
        when 1 then
          L_STRVAL := 'Cтандартное добавление/размножение';
        when 2 then
          L_STRVAL := 'Cтандартное исправление';
        when 3 then
          L_STRVAL := 'Cтандартное удаление';
        when 4 then
          L_STRVAL := 'Cтандартное перемещение (в каталог)';
        when 5 then
          L_STRVAL := 'Cтандартное перемещение (из каталога)';
        when 6 then
          L_STRVAL := 'Стандартное перемещение (в иерархии)';
        when 7 then
          L_STRVAL := 'Заполнение на основе данных раздела';
        when 8 then
          L_STRVAL := 'Стандартный перенос в Excel';
        when 9 then
          L_STRVAL := 'Стандартный экспорт';
        when 10 then
          L_STRVAL := 'Стандартный просмотр спецификации';
        when 11 then
          L_STRVAL := 'Открыть раздел';
        when 12 then
          L_STRVAL := 'Стандартное формирование ЭЦП';
        when 13 then
          L_STRVAL := 'Стандартная проверка ЭЦП';
        when 14 then
          L_STRVAL := 'Стандартное удаление ЭЦП';
        when 15 then
          L_STRVAL := 'Стандартный файловый экспорт';
        when 16 then
          L_STRVAL := 'Стандартный файловый импорт';
        when 17 then
          L_STRVAL := 'Стандартное ослабление контроля связей';
        when 18 then
          L_STRVAL := 'Стандартное восстановление контроля связей';
        when 30 then
          L_STRVAL := 'Пользовательский отчет';
        when 31 then
          L_STRVAL := 'Пользовательское приложение';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Тип', L_STRVAL, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Подчиненный класс', L_ACTION.DETAILCODE, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Код', L_ACTION.CODE, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_ACTION.RN, 'UNITFUNC', 'NAME', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_ACTION.TECHNOLOGY
        when 0 then
          L_STRVAL := 'Стандарт';
        when 1 then
          L_STRVAL := 'Конструктор';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Технология производства', L_STRVAL, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Позиция', L_ACTION.NUMB, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Реализующий метод', L_ACTION.SMETHOD, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Пиктограмма', L_ACTION.SSYSIMAGE, A_LEVEL + 2);
      case L_ACTION.PROCESS_MODE
        when 0 then
          L_STRVAL := 'Не зависит от записей';
        when 1 then
          L_STRVAL := 'Для одной текущей записи';
        when 2 then
          L_STRVAL := 'Для всех помеченных записей';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Обработка записей', L_STRVAL, A_LEVEL + 2);
      case L_ACTION.TRANSACT_MODE
        when 0 then
          L_STRVAL := 'После всех вызовов действия';
        when 1 then
          L_STRVAL := 'После каждого вызова действия';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Завершение транзакции', L_STRVAL, A_LEVEL + 2);
      case L_ACTION.REFRESH_MODE
        when 0 then
          L_STRVAL := 'Не обновлять';
        when 1 then
          L_STRVAL := 'Обновлять только текущую запись';
        when 2 then
          L_STRVAL := 'Обновлять всю выборку';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Обновление выборки', L_STRVAL, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Показывать диалог при отсутствии визуализируемых параметров',
                    INT2BOOL(L_ACTION.SHOW_DIALOG),
                    A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Отображать только при технологии производства «Конструктор»',
                    INT2BOOL(L_ACTION.ONLY_CUSTOM_MODE),
                    A_LEVEL + 2);
      case L_ACTION.OVERRIDE
        when 0 then
          L_STRVAL := 'Нестандартное';
        when 1 then
          L_STRVAL := 'Cтандартное добавление/размножение';
        when 2 then
          L_STRVAL := 'Cтандартное исправление';
        when 3 then
          L_STRVAL := 'Cтандартное удаление';
        when 4 then
          L_STRVAL := 'Cтандартное перемещение (в каталог)';
        when 5 then
          L_STRVAL := 'Cтандартное перемещение (из каталога)';
        when 6 then
          L_STRVAL := 'Стандартное перемещение (в иерархии)';
        when 7 then
          L_STRVAL := 'Заполнение на основе данных раздела';
        when 8 then
          L_STRVAL := 'Стандартный перенос в Excel';
        when 9 then
          L_STRVAL := 'Стандартный экспорт';
        when 10 then
          L_STRVAL := 'Стандартный просмотр спецификации';
        when 11 then
          L_STRVAL := 'Открыть раздел';
        when 12 then
          L_STRVAL := 'Стандартное формирование ЭЦП';
        when 13 then
          L_STRVAL := 'Стандартная проверка ЭЦП';
        when 14 then
          L_STRVAL := 'Стандартное удаление ЭЦП';
        when 15 then
          L_STRVAL := 'Стандартный файловый экспорт';
        when 16 then
          L_STRVAL := 'Стандартный файловый импорт';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Переопределенный тип', L_STRVAL, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION,
                    'Безусловная доступность',
                    INT2BOOL(L_ACTION.UNCOND_ACCESS),
                    A_LEVEL + 2);
      PUT_FORMS(L_ACTION.RN, L_ACTION.CODE);
      PUT_PARAMS(L_ACTION.RN);
      PUT_METHODS(L_ACTION.RN, L_ACTION.CODE);
      PUT_STEPS(L_ACTION.RN, L_ACTION.CODE);
    end loop;
    close LC_ACTIONS;
    return L_SECTION;
  end;

  function GET_OBJECTS_SECTION(A_LEVEL in number default 0) return clob is
    --     --     select * from V_DMSCLOBJECTS where NPRN=625379  order by SNAME desc
    cursor LC_OBJECTS is
      select T.RN,
             T.OBJTYPE,
             T.NAME,
             T.OBJKIND,
             T.PLSQL_TEXT
        from DMSCLOBJECTS T
       where T.PRN = WORKIN_CLASS
       order by T.NAME;
    L_OBJECT    LC_OBJECTS%rowtype;
    L_SECTION   clob;
    L_TEXT_RU   PKG_STD.TSTRING;
    L_TEXT_UK   PKG_STD.TSTRING;
    L_STRVAL    PKG_STD.TSTRING;
    L_FILENAME  PKG_STD.TSTRING;
    L_EXTENSION PKG_STD.TSTRING;
  begin
    PUT_LINE(L_SECTION, '');
    PUT_LINE(L_SECTION, '# Объекты');
    PUT_TOML_TABLE(L_SECTION, 'Класс.Объекты', false, A_LEVEL);
    open LC_OBJECTS;
    loop
      fetch LC_OBJECTS
        into L_OBJECT;
      exit when LC_OBJECTS%notfound;
      PUT_LINE(L_SECTION, '');
      PUT_TOML_TABLE(L_SECTION, 'Объект', true, A_LEVEL + 1);
      case L_OBJECT.OBJTYPE
        when 0 then
          L_STRVAL    := 'Таблица';
          L_EXTENSION := '.sql';
        when 1 then
          L_STRVAL    := 'Индекс';
          L_EXTENSION := '.sql';
        when 2 then
          L_STRVAL    := 'Триггер';
          L_EXTENSION := '.trg';
        when 3 then
          L_STRVAL    := 'Процедура';
          L_EXTENSION := '.prc';
        when 4 then
          L_STRVAL    := 'Функция';
          L_EXTENSION := '.fnc';
        when 5 then
          L_STRVAL    := 'Пакет';
          L_EXTENSION := '.pck';
        when 6 then
          L_STRVAL    := 'Пакет (тело)';
          L_EXTENSION := '.pkb';
        when 7 then
          L_STRVAL    := 'Представление';
          L_EXTENSION := '.vw';
        when 8 then
          L_STRVAL    := 'Последовательность';
          L_EXTENSION := '.sql';
        when 9 then
          L_STRVAL    := 'Внешние ключи';
          L_EXTENSION := '.sql';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Тип', L_STRVAL, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Имя', L_OBJECT.NAME, A_LEVEL + 2);
      GET_RESOURCES_RU_UK(L_OBJECT.RN, 'DMSCLOBJECTS', 'CAPTION', L_TEXT_RU, L_TEXT_UK);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (RU)', L_TEXT_RU, A_LEVEL + 2);
      PUT_TOML_PAIR(L_SECTION, 'Наименование (UK)', L_TEXT_UK, A_LEVEL + 2);
      case L_OBJECT.OBJKIND
        when 0 then
          L_STRVAL := 'Базовый';
        when 1 then
          L_STRVAL := 'Клиентский';
        when 2 then
          L_STRVAL := 'Полный клиентский';
        else
          L_STRVAL := null;
      end case;
      PUT_TOML_PAIR(L_SECTION, 'Вид', L_STRVAL, A_LEVEL + 2);
      if L_OBJECT.PLSQL_TEXT is not null and LENGTH(L_OBJECT.PLSQL_TEXT) > 0 then
        L_FILENAME := 'Object_' || L_OBJECT.NAME || L_EXTENSION;
        PUT_TOML_PAIR(L_SECTION, 'Исходный текст', L_FILENAME, A_LEVEL + 2);
        ADD_BUFF_CLOB(L_FILENAME, L_OBJECT.PLSQL_TEXT);
      end if;
    end loop;
    close LC_OBJECTS;
    return L_SECTION;
  end;

  procedure EXPORT_METADATA is
    L_METADATA clob;
  begin
    PUT_CLOB(L_METADATA, GET_DOMAINS_SECTION);
    PUT_CLOB(L_METADATA, GET_CLASS_SECTION);
    PUT_CLOB(L_METADATA, GET_ATTRIBUTES_SECTION(1));
    PUT_CLOB(L_METADATA, GET_CONSTRAINTS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_LINKS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_VIEWS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_METHODS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_SHOWMETODS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_ACTIONS_SECTION(1));
    PUT_CLOB(L_METADATA, GET_OBJECTS_SECTION(1));
    ADD_BUFF_BLOB('Metadata.toml', CLOB_TO_BLOB(L_METADATA, 'UTF8'));
  end;

  procedure EXPORT_ICONS is
    cursor LC_ICONS is
      select SY.*
        from SYSIMAGES SY
       where RN in (
                    
                    select T.RN
                      from SYSIMAGES T
                     where exists (select null
                              from UNITLIST UL
                             where UL.RN = WORKIN_CLASS
                               and UL.SYSIMAGE = T.RN)
                    union
                    select T.RN
                      from SYSIMAGES T
                     where exists (select null
                              from UNITFUNC UF
                             where UF.PRN = WORKIN_CLASS
                               and UF.SYSIMAGE = T.RN)
                    union
                    select T.RN
                      from SYSIMAGES T
                     where exists (select null
                              from UNIT_SHOWMETHODS US
                             where US.PRN = WORKIN_CLASS
                               and US.SYSIMAGE = T.RN));
    L_ICON LC_ICONS%rowtype;
  begin
    open LC_ICONS;
    loop
      fetch LC_ICONS
        into L_ICON;
      exit when LC_ICONS%notfound;
      ADD_BUFF_BLOB('Icon_' || L_ICON.CODE || '_16.bmp', L_ICON.SMALL_IMAGE);
      ADD_BUFF_BLOB('Icon_' || L_ICON.CODE || '_24.bmp', L_ICON.LARGE_IMAGE);
    end loop;
    close LC_ICONS;
  end;

  procedure SET_ENV
  (
    A_CLASS_RN       in number,
    A_FILEBUFF_IDENT in number
  ) is
  begin
    WORKIN_CLASS      := A_CLASS_RN;
    FILE_BUFFER_IDENT := A_FILEBUFF_IDENT;
  end;

  procedure EXPORT_CLASS_FULL
  (
    A_CLASS_RN       in number,
    A_FILEBUFF_IDENT in number
  ) is
  begin
    SET_ENV(A_CLASS_RN, A_FILEBUFF_IDENT);
    EXPORT_ICONS;
    EXPORT_METADATA;
  end;

end UDO_PKG_DMSCLASS_DIFF_EXPORT;
/
