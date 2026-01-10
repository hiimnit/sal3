codeunit 66030 "sal3 Runner"
{
    SingleInstance = true;

    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Cell: Interface "sal3 Form Cell";
        Symbol: Interface "sal3 Form Symbol";
        Function: Interface "sal3 Form";
    begin
        case true of
            Form is "sal3 Form Cell":
                begin
                    Cell := Form as "sal3 Form Cell";

                    if not (Cell.Car is "sal3 Form Symbol") then
                        Error('Invalid function call: %1 is not a symbol.', Cell.Car.ToString());

                    Symbol := Cell.Car as "sal3 Form Symbol";
                    Function := Env.Get(Symbol.Name);
                    // TODO checks?

                    Error('TODO');
                end;
            Form is "sal3 Form Symbol":
                begin
                    Symbol := Form as "sal3 Form Symbol";
                    exit(Env.Get(Symbol.Name))
                end;
            Form is "sal3 Form Number",
            Form is "sal3 Form String",
            Form is "sal3 Form Nil":
                exit(Form);
            else
                Error('Format not implemented for %1.', Form.ToString());
        end;
    end;
}

codeunit 66031 "sal3 Environment"
{
    var
        // TODO parent env
        Bindings: Dictionary of [Text, Interface "sal3 Form"];

    procedure Initialize()
    var
        myInt: Integer;
    begin
        Define('defun', 'TODO');

        Define('+', 'TODO');
        Define('-', 'TODO');
        Define('*', 'TODO');
        Define('/', 'TODO');
        // TODO mod div
    end;

    procedure Define(Name: Text; Value: Interface "sal3 Form")
    begin
        Bindings.Set(Name, Value);
    end;

    procedure Get(Name: Text): Interface "sal3 Form"
    begin
        // TODO parent handling
        exit(Bindings.Get(Name));
    end;
}

// TODO is extending Form just for Bindings to work a good idea?
interface "sal3 Function" extends "sal3 Form"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
}

codeunit 66032 "sal3 User Function"
{
    trigger OnRun()
    begin

    end;

    var
        myInt: Integer;
}

codeunit 66040 "sal3 Function Add" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Number: Codeunit "sal3 Number";
        Cell: Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
        ArgNumber: Interface "sal3 Form Number";
        Result: Decimal;
    begin
        Result := 0;

        if Cell is "sal3 Form Nil" then begin
            Number.Init(Result);
            exit(Number);
        end;

        while Form is "sal3 Form Cell" do begin
            Cell := Form as "sal3 Form Cell";

            Arg := Runner.Eval(Cell.Car, Env);
            if not (Arg is "sal3 Form Number") then
                Error('Invalid + argument: Expected number, got %1.', Arg.ToString());

            ArgNumber := Arg as "sal3 Form Number";
            Result += ArgNumber.Value;

            Form := Cell.Cdr;
        end;

        if not (Form is "sal3 Form Nil") then
            Error('Invalid +: Arguments are not a list.');

        Number.Init(Result);
        exit(Number);
    end;
}