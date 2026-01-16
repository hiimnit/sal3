codeunit 66011 "sal3 Parser"
{
    var
        Lexer: Codeunit "sal3 Lexer";
        Forms: Codeunit "sal3 Forms";
        Lexemes: Codeunit "sal3 Lexemes";

    procedure Parse(InText: Text): List of [Interface "sal3 Form"]
    var
        Forms: List of [Interface "sal3 Form"];
    begin
        Lexer.Init(InText);

        while not (Lexer.Peek() is "sal3 Lexeme EOS") do
            Forms.Add(ParseForm());

        exit(Forms);
    end;

    procedure ParseForm(): Interface "sal3 Form"
    var
        Lexeme: Interface "sal3 Lexeme";
        Form: Interface "sal3 Form";
    begin
        Lexeme := Lexer.Next();

        case true of
            Lexeme is "sal3 Lexeme EOS":
                Error('Unexpected EOS when parsing form.');
            Lexeme is "sal3 Lexeme Open Paren":
                begin
                    Form := ParseListBody();

                    Lexeme := Lexer.Next();
                    if not (Lexeme is "sal3 Lexeme Close Paren") then
                        Error('Expected %1, got %2 instead at position %3.', '<)>', Lexeme.ToString(), Lexer.GetPosition());

                    exit(Form);
                end;
            Lexeme is "sal3 Lexeme Quote":
                exit(Forms.Cell(
                    Lexemes.Symbol('quote'),
                    Forms.Cell(ParseForm(), Forms.Nil())
                ));
            Lexeme is "sal3 Lexeme Number",
            Lexeme is "sal3 Lexeme String",
            Lexeme is "sal3 Lexeme Symbol":
                exit(Lexeme as "sal3 Form");
            else
                Error('Unexpected lexeme %1 at position %2.', Lexeme.ToString(), Lexer.GetPosition());
        end;
    end;

    local procedure ParseListBody(): Interface "sal3 Form"
    var
        Lexeme: Interface "sal3 Lexeme";
        Form1, Form2 : Interface "sal3 Form";
    begin
        Lexeme := Lexer.Peek();
        if Lexeme is "sal3 Lexeme Close Paren" then
            exit(Forms.Nil());

        Form1 := ParseForm();

        Lexeme := Lexer.Peek();
        if Lexeme is "sal3 Lexeme Dot" then begin
            Lexer.Next();

            Form2 := ParseForm();

            exit(Forms.Cell(Form1, Form2));
        end;

        Form2 := ParseListBody();

        exit(Forms.Cell(Form1, Form2));
    end;
}

codeunit 66012 "sal3 Forms"
{
    procedure Nil(): Codeunit "sal3 Nil"
    begin
    end;

    procedure Cell(InCar: Interface "sal3 Form"; InCdr: Interface "sal3 Form") Form: Codeunit "sal3 Cell"
    begin
        Form.Init(InCar, InCdr);
    end;

    procedure Bool(InBool: Boolean) Form: Codeunit "sal3 Boolean"
    begin
        Form.Init(InBool);
    end;
}

interface "sal3 Form" extends "sal3 Form Or Function"
{
    procedure ToString(): Text;
}

interface "sal3 Form Cell"
{
    procedure Car(): Interface "sal3 Form";
    procedure Cdr(): Interface "sal3 Form";
}

codeunit 66020 "sal3 Cell" implements "sal3 Form", "sal3 Form Cell"
{
    var
        CarForm, CdrForm : Interface "sal3 Form";

    procedure Init(InCar: Interface "sal3 Form"; InCdr: Interface "sal3 Form")
    begin
        CarForm := InCar;
        CdrForm := InCdr;
    end;

    procedure ToString(): Text;
    begin
        exit(StrSubstNo('(%1 . %2)', CarForm.ToString(), CdrForm.ToString()));
    end;

    procedure Car(): Interface "sal3 Form"
    begin
        exit(CarForm);
    end;

    procedure Cdr(): Interface "sal3 Form"
    begin
        exit(CdrForm);
    end;
}

interface "sal3 Form Nil" { }

codeunit 66021 "sal3 Nil" implements "sal3 Form", "sal3 Form Nil"
{
    SingleInstance = true;

    procedure ToString(): Text;
    begin
        exit('nil');
    end;
}