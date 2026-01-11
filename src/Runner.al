codeunit 66030 "sal3 Runner"
{
    SingleInstance = true;

    procedure EvalForms
    (
        Forms: List of [Interface "sal3 Form"]
    ): Interface "sal3 Form"
    var
        Env: Codeunit "sal3 Environment";
        Form, Result : Interface "sal3 Form";
    begin
        Env.Initialize();

        foreach Form in Forms do
            Result := Eval(Form, Env);

        exit(Result);
    end;

    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Cell: Interface "sal3 Form Cell";
        Symbol: Interface "sal3 Form Symbol";
        Function: Interface "sal3 Function";
    begin
        case true of
            Form is "sal3 Form Cell":
                begin
                    Cell := Form as "sal3 Form Cell";

                    if not (Cell.Car is "sal3 Form Symbol") then
                        Error('Invalid function call: %1 is not a symbol.', Cell.Car.ToString());

                    Symbol := Cell.Car as "sal3 Form Symbol";
                    Function := Env.GetFunction(Symbol.Name);

                    exit(Function.Eval(Cell.Cdr, Env));
                end;
            Form is "sal3 Form Symbol":
                begin
                    Symbol := Form as "sal3 Form Symbol";
                    exit(Env.GetForm(Symbol.Name))
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

interface "sal3 Form Or Function" { }

codeunit 66031 "sal3 Environment"
{
    var
        // TODO parent env
        // TODO  - global vs local?

        Bindings: Dictionary of [Text, Interface "sal3 Form Or Function"];

    procedure Initialize()
    var
        Defun: Codeunit "sal3 Function Defun";
        Add: Codeunit "sal3 Function Add";
        Car: Codeunit "sal3 Function Car";
        Cdr: Codeunit "sal3 Function Cdr";
        Forms: Codeunit "sal3 Forms";
    begin
        Define('defun', Defun);

        // TODO cons, list
        Define('car', Car);
        Define('cdr', Cdr);

        Define('+', Add);
        // TODO Define('-', 'TODO');
        // TODO Define('*', 'TODO');
        // TODO Define('/', 'TODO');
        // TODO mod div

        // TODO constants - t
        Define('nil', Forms.Nil);
    end;

    procedure Define(Name: Text; Value: Interface "sal3 Form")
    begin
        Bindings.Set(Name, Value);
    end;

    procedure Define(Name: Text; Value: Interface "sal3 Function")
    begin
        Bindings.Set(Name, Value);
    end;

    procedure Get(Name: Text): Interface "sal3 Form Or Function"
    begin
        // XXX if Bindings.Get(Name, Result) then exit(Result); fails with a compilation error
        if Bindings.ContainsKey(Name) then
            exit(Bindings.Get(Name));

        // TODO parent handling
        Error('%1 is not defined.', Name);
    end;

    procedure GetForm(Name: Text): Interface "sal3 Form"
    var
        FormOrFunction: Interface "sal3 Form Or Function";
    begin
        FormOrFunction := Get(Name);
        if not (FormOrFunction is "sal3 Form") then
            Error('%1 is not a form.', Name);
        exit(FormOrFunction as "sal3 Form");
    end;

    procedure GetFunction(Name: Text): Interface "sal3 Function"
    var
        FormOrFunction: Interface "sal3 Form Or Function";
    begin
        FormOrFunction := Get(Name);
        if not (FormOrFunction is "sal3 Function") then
            Error('%1 is not a function.', Name);
        exit(FormOrFunction as "sal3 Function");
    end;
}

interface "sal3 Function" extends "sal3 Form Or Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
}

codeunit 66032 "sal3 Defined Function"
{
    trigger OnRun()
    begin

    end;

    var
        myInt: Integer;
}

codeunit 66039 "sal3 Function Defun" implements "sal3 Function"
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
        // (defun name (args) (body))
        Error('TODO');
    end;
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

codeunit 66041 "sal3 Function Car" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Number: Codeunit "sal3 Number";
        Cell, ArgCell : Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
    begin
        if not (Form is "sal3 Form Cell") then
            Error('Invalid car argument: Expected single cons cell, got %1.', Form.ToString());

        Cell := Form as "sal3 Form Cell";
        if not (Cell.Cdr is "sal3 Form Nil") then
            Error('Invalid car argument: Expected exactly one argument.');

        Arg := Runner.Eval(Cell.Car, Env);
        if not (Arg is "sal3 Form Cell") then
            Error('Invalid car argument: Expected cons cell, got %1.', Arg.ToString());

        ArgCell := Arg as "sal3 Form Cell";
        exit(ArgCell.Car);
    end;
}

codeunit 66042 "sal3 Function Cdr" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Number: Codeunit "sal3 Number";
        Cell, ArgCell : Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
    begin
        if not (Form is "sal3 Form Cell") then
            Error('Invalid cdr argument: Expected single cons cell, got %1.', Form.ToString());

        Cell := Form as "sal3 Form Cell";
        if not (Cell.Cdr is "sal3 Form Nil") then
            Error('Invalid cdr argument: Expected exactly one argument.');

        Arg := Runner.Eval(Cell.Car, Env);
        if not (Arg is "sal3 Form Cell") then
            Error('Invalid cdr argument: Expected cons cell, got %1.', Arg.ToString());

        ArgCell := Arg as "sal3 Form Cell";
        exit(ArgCell.Cdr);
    end;
}