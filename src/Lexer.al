codeunit 66000 "sal3 Lexer"
{
    var
        Lexemes: Codeunit "sal3 Lexemes";
        Text: Text;
        Position: Integer;
        asdf: Dictionary of [Integer, Codeunit "sal3 Lexer"];

    procedure Init(InText: Text)
    begin
        Text := InText;
        Position := 0;
        HasPeekedLexeme := false;
    end;

    procedure GetPosition(): Integer
    begin
        exit(Position);
    end;

    procedure Next(): Interface "sal3 Lexeme"
    var
        c: Char;
    begin
        if HasPeekedLexeme then begin
            HasPeekedLexeme := false;
            exit(PeekedLexeme);
        end;

        repeat
            c := NextChar();
            if c = 0 then
                exit(Lexemes.EOS);
        until not IsWhiteSpace(c);

        case true of
            // FIXME comments
            c = '(':
                exit(Lexemes.OpenParenthesis);
            c = ')':
                exit(Lexemes.CloseParenthesis);
            c = '''':
                exit(Lexemes.Quote);
            c = '.':
                exit(ParseDot());
            c = '-':
                exit(ParseDash());
            c in ['0' .. '9']:
                exit(ParseNumberLiteral(c, true, false));
            c = '"':
                exit(ParseStringLiteral());
            else
                exit(ParseSymbol(c));
        end;
    end;

    local procedure NextChar(): Char
    begin
        if Position >= StrLen(Text) then
            exit(0);

        Position += 1;
        exit(Text[Position]);
    end;

    local procedure PushChar()
    begin
        Position -= 1;
    end;

    local procedure IsWhiteSpace(c: Char): Boolean
    begin
        // space, tab, lf, cr, tab, nonbreaking space
        exit(c in [' ', 9, 10, 11, 13, 160]);
    end;

    local procedure IsSymbolSeparator(c: Char): Boolean
    begin
        exit(c in ['(', ')', '''', '"']);
    end;

    local procedure ParseDot(): Interface "sal3 Lexeme"
    var
        c: Char;
    begin
        c := NextChar();

        case true of
            IsSymbolSeparator(c):
                begin
                    PushChar();
                    exit(Lexemes.Dot);
                end;
            c = 0,
            IsWhitespace(c):
                exit(Lexemes.Dot);
            c in ['0' .. '9']:
                exit(ParseNumberLiteral(c, true, true));
            else
                exit(ParseSymbol('.' + Format(c)));
        end;
    end;

    local procedure ParseDash(): Interface "sal3 Lexeme"
    var
        c: Char;
    begin
        c := NextChar();

        case true of
            IsSymbolSeparator(c):
                begin
                    PushChar();
                    exit(Lexemes.Symbol('-'));
                end;
            c = 0,
            IsWhitespace(c):
                exit(Lexemes.Symbol('-'));
            c in ['0' .. '9']:
                exit(ParseNumberLiteral(c, false, true));
            c = '.':
                begin
                    c := NextChar();

                    case true of
                        IsSymbolSeparator(c):
                            begin
                                PushChar();
                                exit(Lexemes.Symbol('-.'));
                            end;
                        c = 0,
                        IsWhitespace(c):
                            exit(Lexemes.Symbol('-.'));
                        c in ['0' .. '9']:
                            exit(ParseNumberLiteral(c, false, true));
                        else
                            exit(ParseSymbol('-.' + Format(c)));
                    end;
                end;
            else
                exit(ParseSymbol('-' + Format(c)));
        end;
    end;

    local procedure ParseNumberLiteral
    (
        c: Char;
        Positive: Boolean;
        DecimalSeparatorFound: Boolean
    ): Interface "sal3 Lexeme"
    var
        Number: Decimal;
        SymbolBuilder: TextBuilder;
        DecimalPlaces: Integer;
    begin
        Number := c - '0';
        SymbolBuilder.Append(c);

        while true do begin
            c := NextChar();

            if c in ['0' .. '9'] then begin
                SymbolBuilder.Append(c);

                if DecimalSeparatorFound then begin
                    Number := Number + (c - '0') / Power(10, DecimalPlaces);
                    DecimalPlaces += 1;
                end else
                    Number := Number * 10 + c - '0';

                continue;
            end;

            if c = '.' then begin
                SymbolBuilder.Append(c);

                if DecimalSeparatorFound then
                    exit(ParseSymbol(SymbolBuilder));

                DecimalSeparatorFound := true;
                DecimalPlaces := 1;
            end;

            if IsSymbolSeparator(c) then begin
                PushChar();
                break;
            end;
            if c = 0 then
                break;
            if IsWhiteSpace(c) then
                break;
        end;

        exit(Lexemes.Number(Positive ? Number : -Number));
    end;

    local procedure ParseStringLiteral(): Codeunit "sal3 String"
    var
        StringBuilder: TextBuilder;
        c: Char;
        Escaping: Boolean;
    begin
        while true do begin
            c := NextChar();
            if c = 0 then
                Error('Unexpected EOS in string literal.');

            if not Escaping then begin
                if c = '\' then begin
                    Escaping := true;
                    continue;
                end;

                if c = '"' then
                    break;
            end;

            StringBuilder.Append(c);
            Escaping := false;
        end;

        exit(Lexemes.String(StringBuilder.ToText()));
    end;

    local procedure ParseSymbol(c: Text): Codeunit "sal3 Symbol"
    var
        SymbolBuilder: TextBuilder;
    begin
        SymbolBuilder.Append(c);
        exit(ParseSymbol(SymbolBuilder));
    end;

    local procedure ParseSymbol(SymbolBuilder: TextBuilder): Codeunit "sal3 Symbol"
    var
        c: Char;
    begin
        while true do begin
            c := NextChar();

            if c in ['(', ')', '''', '"'] then begin
                PushChar();
                break;
            end;
            if c = 0 then
                break;
            if IsWhiteSpace(c) then
                break;

            SymbolBuilder.Append(c);
        end;

        exit(Lexemes.Symbol(SymbolBuilder.ToText()));
    end;

    var
        PeekedLexeme: Interface "sal3 Lexeme";
        HasPeekedLexeme: Boolean;

    procedure Peek(): Interface "sal3 Lexeme"
    begin
        if HasPeekedLexeme then
            exit(PeekedLexeme);

        PeekedLexeme := Next();
        HasPeekedLexeme := true;

        exit(PeekedLexeme);
    end;
}

codeunit 66001 "sal3 Lexemes"
{
    procedure EOS(): Codeunit "sal3 EOS"
    begin
    end;

    procedure OpenParenthesis(): Codeunit "sal3 Open Parenthesis"
    begin
    end;

    procedure CloseParenthesis(): Codeunit "sal3 Close Parenthesis"
    begin
    end;

    procedure Quote(): Codeunit "sal3 Quote"
    begin
    end;

    procedure Dot(): Codeunit "sal3 Dot"
    begin
    end;

    procedure Number(InNumber: Decimal) Lexeme: Codeunit "sal3 Number"
    begin
        Lexeme.Init(InNumber);
    end;

    procedure String(InString: Text) Lexeme: Codeunit "sal3 String"
    begin
        Lexeme.Init(InString);
    end;

    procedure Symbol(InSymbol: Text) Lexeme: Codeunit "sal3 Symbol"
    begin
        Lexeme.Init(InSymbol);
    end;
}

interface "sal3 Lexeme"
{
    procedure ToString(): Text
}

interface "sal3 Lexeme EOS" { }

codeunit 66002 "sal3 EOS" implements "sal3 Lexeme", "sal3 Lexeme EOS"
{
    SingleInstance = true;

    procedure ToString(): Text
    begin
        exit('<EOS>');
    end;
}

interface "sal3 Lexeme Open Paren" { }

codeunit 66003 "sal3 Open Parenthesis" implements "sal3 Lexeme", "sal3 Lexeme Open Paren"
{
    SingleInstance = true;

    procedure ToString(): Text
    begin
        exit('<(>');
    end;
}
interface "sal3 Lexeme Close Paren" { }

codeunit 66004 "sal3 Close Parenthesis" implements "sal3 Lexeme", "sal3 Lexeme Close Paren"
{
    SingleInstance = true;

    procedure ToString(): Text
    begin
        exit('<)>');
    end;
}

interface "sal3 Lexeme Quote" { }

codeunit 66005 "sal3 Quote" implements "sal3 Lexeme", "sal3 Lexeme Quote"
{
    SingleInstance = true;

    procedure ToString(): Text
    begin
        exit('<''>');
    end;
}

interface "sal3 Lexeme Number" { }
interface "sal3 Form Number"
{
    procedure Value(): Decimal;
}

codeunit 66006 "sal3 Number" implements "sal3 Lexeme", "sal3 Lexeme Number", "sal3 Form", "sal3 Form Number"
{
    var
        Number: Decimal;

    procedure Init(InNumber: Decimal)
    begin
        Number := InNumber;
    end;

    procedure ToString(): Text
    begin
        exit(StrSubstNo('<Number="%1">', Number));
    end;

    procedure Value(): Decimal
    begin
        exit(Number);
    end;

    procedure Unwrap(): Variant
    begin
        exit(Number);
    end;
}

interface "sal3 Lexeme String" { }
interface "sal3 Form String"
{
    procedure Value(): Text;
}

codeunit 66007 "sal3 String" implements "sal3 Lexeme", "sal3 Lexeme String", "sal3 Form", "sal3 Form String"
{
    var
        String: Text;

    procedure Init(InString: Text)
    begin
        String := InString;
    end;

    procedure ToString(): Text
    begin
        exit(StrSubstNo('<String="%1">', String));
    end;

    procedure Value(): Text
    begin
        exit(String);
    end;

    procedure Unwrap(): Variant
    begin
        exit(String);
    end;
}

interface "sal3 Lexeme Symbol" { }
interface "sal3 Form Symbol"
{
    procedure Name(): Text;
}

codeunit 66008 "sal3 Symbol" implements "sal3 Lexeme", "sal3 Lexeme Symbol", "sal3 Form", "sal3 Form Symbol"
{
    var
        Symbol: Text;

    procedure Init(InSymbol: Text)
    begin
        Symbol := InSymbol;
    end;

    procedure ToString(): Text
    begin
        exit(StrSubstNo('<Symbol="%1">', Symbol));
    end;

    procedure Name(): Text
    begin
        exit(Symbol);
    end;

    procedure Unwrap(): Variant
    begin
        Error('Attempted to unwrap symbol "%1".', Symbol);
    end;
}

interface "sal3 Lexeme Dot" { }

codeunit 66009 "sal3 Dot" implements "sal3 Lexeme", "sal3 Lexeme Dot"
{
    procedure ToString(): Text
    begin
        exit('<.>');
    end;
}

// TODO "sal3 Date"
// TODO "sal3 Time"
// TODO "sal3 DateTime"

interface "sal3 Form Boolean"
{
    procedure Value(): Boolean;
}

codeunit 66010 "sal3 Boolean" implements "sal3 Form", "sal3 Form Boolean"
{
    var
        Bool: Boolean;

    procedure Init(InBool: Boolean)
    begin
        Bool := InBool;
    end;

    procedure ToString(): Text
    begin
        exit(StrSubstNo('<Boolean="%1">', Bool));
    end;

    procedure Value(): Boolean
    begin
        exit(Bool);
    end;

    procedure Unwrap(): Variant
    begin
        exit(Bool);
    end;
}

interface "sal3 Form Record"
{
    procedure Value(): RecordRef;
    procedure FieldValue(FieldName: Text): Interface "sal3 Form";
    procedure FindFieldByName(FieldName: Text): Integer;
}

codeunit 66011 "sal3 Record" implements "sal3 Form", "sal3 Form Record"
{
    var
        RecordRef: RecordRef;

    procedure Init(TableNo: Integer)
    begin
        RecordRef.Open(TableNo);
    end;

    procedure ToString(): Text
    begin
        exit(StrSubstNo('<Record="%1">', RecordRef.Name));
    end;

    procedure Value(): RecordRef
    begin
        exit(RecordRef);
    end;

    procedure Unwrap(): Variant
    begin
        exit(RecordRef);
    end;

    procedure FieldValue(FieldName: Text): Interface "sal3 Form"
    var
        FieldRef: FieldRef;
        Forms: Codeunit "sal3 Forms";
    begin
        FieldRef := RecordRef.Field(FindFieldByName(FieldName));

        case FieldRef.Type of
            FieldType::Text,
            FieldType::Code:
                exit(Forms.String(FieldRef.Value));
            FieldType::Integer,
            FieldType::Decimal:
                exit(Forms.Number(FieldRef.Value));
            FieldType::Boolean:
                exit(Forms.Bool(FieldRef.Value));
            else
                Error('Unsupported field type: Field "%1" of type %2 is not supported.', FieldName, FieldRef.Type);
        end;
    end;

    procedure FindFieldByName(FieldName: Text): Integer
    var
        Field: Record Field;
    begin
        // TODO caching?
        if StrLen(FieldName) > MaxStrLen(Field.FieldName) then
            Error('Invalid field name: Name "%1" is longer than %2 characters.', FieldName, MaxStrLen(Field.FieldName));

        Field.SetRange(TableNo, RecordRef.Number);
        Field.SetRange(FieldName, FieldName);
        if not Field.FindFirst() then
            Error('Invalid field name: Field "%1" does not exist in table "%2".', FieldName, RecordRef.Name);

        exit(Field."No.");
    end;

    // TODO Get - build RecordId and Get?
}