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

    procedure EvalList(List: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Cell: Interface "sal3 Form Cell";
        Symbol: Interface "sal3 Form Symbol";
        Forms: Codeunit "sal3 Forms";
        Result: Interface "sal3 Form";
    begin
        Result := Forms.Nil;

        while not (List is "sal3 Form Nil") do begin
            if not (List is "sal3 Form Cell") then
                Error('Invalid argument: Expected cell, got %1.', List.ToString());
            Cell := List as "sal3 Form Cell";

            Result := Eval(Cell.Car, Env);

            List := Cell.Cdr;
        end;

        exit(Result);
    end;
}

interface "sal3 Form Or Function" { }

codeunit 66031 "sal3 Environment"
{

    var
        Bindings: Dictionary of [Text, Interface "sal3 Form Or Function"];

    procedure Initialize()
    var
        Defun: Codeunit "sal3 Function Defun";
        Let: Codeunit "sal3 Function Let";
        "If": Codeunit "sal3 Function If";
        Add: Codeunit "sal3 Function Add";
        Subtract: Codeunit "sal3 Function Subtract";
        Car: Codeunit "sal3 Function Car";
        Cdr: Codeunit "sal3 Function Cdr";
        RecordOpen: Codeunit "sal3 Function Record Open";
        FieldValue: Codeunit "sal3 Function Field Value";
        Forms: Codeunit "sal3 Forms";
    begin
        Define('if', "If");

        Define('defun', Defun);
        Define('let', Let);
        // TODO quote
        // TODO progn

        // TODO cons, list
        Define('car', Car);
        Define('cdr', Cdr);

        Define('+', Add);
        Define('-', Subtract);
        // TODO Define('*', 'TODO');
        // TODO Define('/', 'TODO');
        // TODO mod, div
        // TODO round

        DefineComparator('=');
        DefineComparator('/=');
        DefineComparator('<');
        DefineComparator('>');
        DefineComparator('<=');
        DefineComparator('>=');

        Define('nil', Forms.Nil);
        Define('t', Forms.Bool(true));

        Define('record-open', RecordOpen);
        DefineRecordFind('record-findfirst');
        DefineRecordFind('record-findfirst-return');
        DefineRecordFind('record-findlast');
        DefineRecordFind('record-findlast-return');
        DefineRecordFind('record-findset');
        DefineRecordFind('record-findset-return');
        DefineRecordFind('record-next');
        DefineRecordFilter('record-setrange');
        DefineRecordFilter('record-setfilter');

        Define('field-value', FieldValue);
    end;

    local procedure DefineComparator(Operator: Text)
    var
        Cmp: Codeunit "sal3 Function Comparison";
    begin
        Define(Operator, Cmp.Init(Operator));
    end;

    local procedure DefineRecordFind(Name: Text)
    var
        Find: Codeunit "sal3 Function Record FindX";
    begin
        Define(Name, Find.Init(Name));
    end;

    local procedure DefineRecordFilter(Name: Text)
    var
        Filter: Codeunit "sal3 Function Record Filter";
    begin
        Define(Name, Filter.Init(Name));
    end;

    var
        ParentEnv: Codeunit "sal3 Environment";
        HasParent: Boolean;

    procedure Create(): Codeunit "sal3 Environment"
    var
        ChildEnv: Codeunit "sal3 Environment";
    begin
        ChildEnv._SetParent(this);
        exit(ChildEnv);
    end;

    procedure _SetParent(InParentEnv: Codeunit "sal3 Environment")
    begin
        ParentEnv := InParentEnv;
        HasParent := true;
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

        if HasParent then
            exit(ParentEnv.Get(Name));

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

    procedure IsTruthy(Form: Interface "sal3 Form"): Boolean
    begin
        if Form is "sal3 Form Nil" then
            exit(false);
        if Form is "sal3 Form Boolean" then
            exit((Form as "sal3 Form Boolean").Value);
        exit(true);
    end;

    procedure Cast
    (
        Form: Interface "sal3 Form";
        FunctionName: Text
    ): Interface "sal3 Form Cell"
    begin
        if not (Form is "sal3 Form Cell") then
            Error('Invalid %1 argument: Expected cons cell, got %2.', FunctionName, Form.ToString());
        exit(Form as "sal3 Form Cell");
    end;

    procedure CastString
    (
        Form: Interface "sal3 Form";
        FunctionName: Text
    ): Interface "sal3 Form String"
    begin
        if not (Form is "sal3 Form String") then
            Error('Invalid %1 argument: Expected string, got %2.', FunctionName, Form.ToString());
        exit(Form as "sal3 Form String");
    end;

    procedure CastRecord
    (
        Form: Interface "sal3 Form";
        FunctionName: Text
    ): Interface "sal3 Form Record"
    begin
        if not (Form is "sal3 Form Record") then
            Error('Invalid %1 argument: Expected record, got %2.', FunctionName, Form.ToString());
        exit(Form as "sal3 Form Record");
    end;

    procedure AssertNilInvalidNoArgs
    (
        Form: Interface "sal3 Form";
        FunctionName: Text
    )
    begin
        if not (Form is "sal3 Form Nil") then
            Error('Invalid number of arguments for %1.', FunctionName);
    end;
}

interface "sal3 Function" extends "sal3 Form Or Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
}

codeunit 66032 "sal3 Defined Function" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        FunctionEnv: Codeunit "sal3 Environment";
        Runner: Codeunit "sal3 Runner";
    begin
        FunctionEnv := Env.Create();

        EvalArguments(Runner, Form, Env, FunctionEnv);

        exit(Runner.EvalList(Body, FunctionEnv));
    end;

    local procedure EvalArguments
    (
        Runner: Codeunit "sal3 Runner";
        Form: Interface "sal3 Form";
        Env: Codeunit "sal3 Environment";
        FunctionEnv: Codeunit "sal3 Environment"
    )
    var
        ParameterName: Text;
        Value: Interface "sal3 Form";
        Cell: Interface "sal3 Form Cell";
    begin
        foreach ParameterName in ParameterNames do begin
            if not (Form is "sal3 Form Cell") then
                Error('Invalid number of arguments when calling %1.', Name);
            Cell := Form as "sal3 Form Cell";

            Value := Runner.Eval(Cell.Car, Env);
            FunctionEnv.Define(ParameterName, Value);

            Form := Cell.Cdr;
        end;

        if not (Form is "sal3 Form Nil") then
            Error('Invalid number of arguments when calling %1.', Name);
    end;

    procedure Initialize
    (
        InName: Text;
        InParameterNames: List of [Text];
        InBody: Interface "sal3 Form"
    )
    begin
        Name := InName;
        ParameterNames := InParameterNames;
        Body := InBody;
    end;

    var
        Name: Text;
        ParameterNames: List of [Text];
        Body: Interface "sal3 Form";
}

codeunit 66039 "sal3 Function Defun" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        DefinedFunction: Codeunit "sal3 Defined Function";
        Cell: Interface "sal3 Form Cell";
        Symbol: Interface "sal3 Form Symbol";
        FunctionBody: Interface "sal3 Form";
        FunctionName: Text;
        FunctionParameterNames: List of [Text];
    begin
        if not (Form is "sal3 Form Cell") then
            Error('Invalid defun argument: Expected function name, got %1.', Form.ToString());
        Cell := Form as "sal3 Form Cell";

        if not (Cell.Car is "sal3 Form Symbol") then
            Error('Invalid defun argument: Expected function name, got %1.', Cell.Car.ToString());
        Symbol := Cell.Car as "sal3 Form Symbol";

        FunctionName := Symbol.Name;

        if not (Cell.Cdr is "sal3 Form Cell") then
            Error('Invalid defun argument: Expected list of parameters, got %1.', Cell.Cdr.ToString());
        Cell := Cell.Cdr as "sal3 Form Cell";

        FunctionParameterNames := ParseParameterNames(Cell.Car);

        FunctionBody := Cell.Cdr;

        DefinedFunction.Initialize(
            FunctionName,
            FunctionParameterNames,
            FunctionBody
        );

        Env.Define(FunctionName, DefinedFunction);

        exit(Symbol as "sal3 Form");
    end;

    local procedure ParseParameterNames
    (
        Parameter: Interface "sal3 Form"
    ): List of [Text]
    var
        Cell: Interface "sal3 Form Cell";
        ParameterName: Text;
        FunctionParameterNames: List of [Text];
    begin
        while not (Parameter is "sal3 Form Nil") do begin
            if not (Parameter is "sal3 Form Cell") then
                Error('Invalid defun argument: Expected list of parameter names, got %1.', Parameter.ToString());
            Cell := Parameter as "sal3 Form Cell";

            if not (Cell.Car is "sal3 Form Symbol") then
                Error('Invalid defun argument: Expected parameter name, got %1.', Cell.Car.ToString());

            ParameterName := (Cell.Car as "sal3 Form Symbol").Name;

            if FunctionParameterNames.Contains(ParameterName) then
                Error('Parameter names must be unique: Duplicate parameter %1.', ParameterName);
            FunctionParameterNames.Add(ParameterName);

            Parameter := Cell.Cdr;
        end;

        exit(FunctionParameterNames);
    end;
}
codeunit 66040 "sal3 Function Let" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        ChildEnv: Codeunit "sal3 Environment";
        Runner: Codeunit "sal3 Runner";
        Cell, ArgCell : Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
    begin
        Cell := Env.Cast(Form, 'let');

        ChildEnv := Env.Create();
        AssignVariables(
            Runner,
            Cell.Car,
            Env,
            ChildEnv
        );

        exit(Runner.EvalList(Cell.Cdr, ChildEnv));
    end;

    local procedure AssignVariables
    (
        Runner: Codeunit "sal3 Runner";
        Form: Interface "sal3 Form";
        Env: Codeunit "sal3 Environment";
        ChildEnv: Codeunit "sal3 Environment"
    )
    var
        Cell: Interface "sal3 Form Cell";
        VariableName: Text;
    begin
        while not (Form is "sal3 Form Nil") do begin
            Cell := Env.Cast(Form, 'let variables');

            AssignVariable(
                Runner,
                Cell.Car,
                Env,
                ChildEnv
            );

            Form := Cell.Cdr;
        end;
    end;

    local procedure AssignVariable
    (
        Runner: Codeunit "sal3 Runner";
        Form: Interface "sal3 Form";
        Env: Codeunit "sal3 Environment";
        ChildEnv: Codeunit "sal3 Environment"
    )
    var
        Cell: Interface "sal3 Form Cell";
        VariableName: Text;
    begin
        Cell := Env.Cast(Form, 'let variable');

        if not (Cell.Car is "sal3 Form Symbol") then
            Error('Invalid let variable argument: Expected variable name, got %1.', Cell.Car.ToString());

        VariableName := (Cell.Car as "sal3 Form Symbol").Name;

        Cell := Env.Cast(Cell.Cdr, 'let variable');

        ChildEnv.Define(
            VariableName,
            Runner.Eval(Cell.Car, Env)
        );

        Env.AssertNilInvalidNoArgs(Cell.Cdr, 'let variable');
    end;
}

codeunit 66041 "sal3 Function Add" implements "sal3 Function"
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

codeunit 66042 "sal3 Function Subtract" implements "sal3 Function"
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
        if not (Form is "sal3 Form Cell") then
            Error('Invalid - argument: Expected cons cell, got %1.', Arg.ToString());
        Cell := Form as "sal3 Form Cell";

        Arg := Runner.Eval(Cell.Car, Env);
        if not (Arg is "sal3 Form Number") then
            Error('Invalid - argument: Expected number, got %1.', Arg.ToString());

        ArgNumber := Arg as "sal3 Form Number";

        Form := Cell.Cdr;
        if Form is "sal3 Form Nil" then begin
            Number.Init(-ArgNumber.Value);
            exit(Number);
        end;

        Result := ArgNumber.Value;

        while Form is "sal3 Form Cell" do begin
            Cell := Form as "sal3 Form Cell";

            Arg := Runner.Eval(Cell.Car, Env);
            if not (Arg is "sal3 Form Number") then
                Error('Invalid - argument: Expected number, got %1.', Arg.ToString());

            ArgNumber := Arg as "sal3 Form Number";
            Result -= ArgNumber.Value;

            Form := Cell.Cdr;
        end;

        if not (Form is "sal3 Form Nil") then
            Error('Invalid -: Arguments are not a list.');

        Number.Init(Result);
        exit(Number);
    end;
}

codeunit 66043 "sal3 Function If" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        ConditionCell, TruthyCell, FalsyCell : Interface "sal3 Form Cell";
        Condition, Truthy, Falsy : Interface "sal3 Form";
        Result: Interface "sal3 Form";
        Forms: Codeunit "sal3 Forms";
    begin
        ConditionCell := Env.Cast(Form, 'if');
        Condition := ConditionCell.Car;

        TruthyCell := Env.Cast(ConditionCell.Cdr, 'if');
        Truthy := TruthyCell.Car;

        if TruthyCell.Cdr is "sal3 Form Nil" then
            Falsy := Forms.Nil
        else begin
            FalsyCell := Env.Cast(TruthyCell.Cdr, 'if');
            Falsy := FalsyCell.Car;
            Env.AssertNilInvalidNoArgs(FalsyCell.Cdr, 'if');
        end;

        Result := Runner.Eval(Condition, Env);
        if Env.IsTruthy(Result) then
            exit(Runner.Eval(Truthy, Env));
        exit(Runner.Eval(Falsy, Env));
    end;
}

codeunit 66044 "sal3 Function Comparison" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Forms: Codeunit "sal3 Forms";
        Cell: Interface "sal3 Form Cell";
        FirstArg, Arg : Interface "sal3 Form";
    begin
        Cell := Env.Cast(Form, Operator);

        FirstArg := Runner.Eval(Cell.Car, Env);

        Form := Cell.Cdr;
        while not (Form is "sal3 Form Nil") do begin
            Cell := Env.Cast(Form, Operator);

            Arg := Runner.Eval(Cell.Car, Env);

            if not Compare(FirstArg, Arg) then
                exit(Forms.Bool(false));

            Form := Cell.Cdr;
        end;

        exit(Forms.Bool(true));
    end;

    var
        Operator: Text;

    procedure Init(InOperator: Text): Codeunit "sal3 Function Comparison"
    begin
        Operator := InOperator;
        exit(this);
    end;

    local procedure Compare(FirstArg: Interface "sal3 Form"; Arg: Interface "sal3 Form"): Boolean
    var
        FirstNumber, Number : Interface "sal3 Form Number";
    begin
        // TODO compare other types 
        FirstNumber := FirstArg as "sal3 Form Number";
        Number := Arg as "sal3 Form Number";

        case Operator of
            '=':
                exit(FirstNumber.Value = Number.Value);
            '/=':
                exit(FirstNumber.Value <> Number.Value);
            '<':
                exit(FirstNumber.Value < Number.Value);
            '>':
                exit(FirstNumber.Value > Number.Value);
            '<=':
                exit(FirstNumber.Value <= Number.Value);
            '>=':
                exit(FirstNumber.Value >= Number.Value);
            else
                Error('Invalid comparison operator %1.', Operator);
        end;
    end;
}

codeunit 66045 "sal3 Function Car" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
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

codeunit 66046 "sal3 Function Cdr" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
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
codeunit 66047 "sal3 Function Record Open" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Forms: Codeunit "sal3 Forms";
        Cell: Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
        StringArg: Interface "sal3 Form String";
    begin
        Cell := Env.Cast(Form, 'record-open');
        Env.AssertNilInvalidNoArgs(Cell.Cdr, 'record-open');

        Arg := Runner.Eval(Cell.Car, Env);
        StringArg := Env.CastString(Arg, 'record-open');

        exit(Forms.Record(StringArg.Value));
    end;
}

codeunit 66048 "sal3 Function Record FindX" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        Cell: Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
    begin
        Cell := Env.Cast(Form, Type);
        Env.AssertNilInvalidNoArgs(Cell.Cdr, Type);

        Arg := Runner.Eval(Cell.Car, Env);

        exit(Find(Env.CastRecord(Arg, Type)));
    end;

    var
        Type: Text;

    procedure Init(InType: Text): Codeunit "sal3 Function Record FindX"
    begin
        Type := InType;
        exit(this);
    end;

    local procedure Find(Record: Interface "sal3 Form Record"): Interface "sal3 Form"
    var
        Forms: Codeunit "sal3 Forms";
        RecordRef: RecordRef;
    begin
        RecordRef := Record.Value();

        case Type of
            'record-findfirst':
                exit(Forms.Bool(RecordRef.FindFirst()));
            'record-findlast':
                exit(Forms.Bool(RecordRef.FindLast()));
            'record-findset':
                exit(Forms.Bool(RecordRef.FindSet()));
            'record-next':
                exit(Forms.Number(RecordRef.Next())); // TODO Steps
            'record-findfirst-return':
                begin
                    RecordRef.FindFirst();
                    exit(Record as "sal3 Form");
                end;
            'record-findlast-return':
                begin
                    RecordRef.FindLast();
                    exit(Record as "sal3 Form");
                end;
            'record-findset-return':
                begin
                    RecordRef.FindSet();
                    exit(Record as "sal3 Form");
                end;
            else
                Error('Invalid Find "%1".', Type);
        end;
    end;
}
codeunit 66049 "sal3 Function Record Filter" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        RecordCell: Interface "sal3 Form Cell";
        FieldNameCell: Interface "sal3 Form Cell";
        FilterCell: Interface "sal3 Form Cell";
        Record, FieldName, Filter : Interface "sal3 Form";
    begin
        RecordCell := Env.Cast(Form, Type);
        FieldNameCell := Env.Cast(RecordCell.Cdr, Type);
        FilterCell := Env.Cast(FieldNameCell.Cdr, Type);
        Env.AssertNilInvalidNoArgs(FilterCell.Cdr, Type); // TODO optional from-to

        Record := Runner.Eval(RecordCell.Car, Env);
        FieldName := Runner.Eval(FieldNameCell.Car, Env);
        Filter := Runner.Eval(FilterCell.Car, Env);

        exit(Filter(
            Env,
            Env.CastRecord(Record, Type),
            Env.CastString(FieldName, Type),
            Filter
        ));
    end;

    var
        Type: Text;

    procedure Init(InType: Text): Codeunit "sal3 Function Record Filter"
    begin
        Type := InType;
        exit(this);
    end;

    local procedure Filter
    (
        Env: Codeunit "sal3 Environment";
        Record: Interface "sal3 Form Record";
        FieldName: Interface "sal3 Form String";
        Filter: Interface "sal3 Form"
    ): Interface "sal3 Form"
    var
        RecordRef: RecordRef;
        FieldRef: FieldRef;
    begin
        RecordRef := Record.Value();
        FieldRef := RecordRef.Field(
            Record.FindFieldByName(FieldName.Value)
        );

        case Type of
            'record-setrange':
                FieldRef.SetRange(Filter.Unwrap());
            'record-setfilter':
                FieldRef.SetFilter(Env.CastString(Filter, Type).Value);
            else
                Error('Invalid Filter "%1".', Type);
        end;

        exit(Record as "sal3 Form");
    end;
}

codeunit 66050 "sal3 Function Field Value" implements "sal3 Function"
{
    procedure Eval(Form: Interface "sal3 Form"; Env: Codeunit "sal3 Environment"): Interface "sal3 Form"
    var
        Runner: Codeunit "sal3 Runner";
        RecordCell, FieldNameCell : Interface "sal3 Form Cell";
        Arg: Interface "sal3 Form";
        RecordArg: Interface "sal3 Form Record";
        StringArg: Interface "sal3 Form String";
    begin
        RecordCell := Env.Cast(Form, 'field-value');
        FieldNameCell := Env.Cast(RecordCell.Cdr, 'field-value');
        Env.AssertNilInvalidNoArgs(FieldNameCell.Cdr, 'field-value');

        Arg := Runner.Eval(RecordCell.Car, Env);
        RecordArg := Env.CastRecord(Arg, 'field-value');

        Arg := Runner.Eval(FieldNameCell.Car, Env);
        StringArg := Env.CastString(Arg, 'field-value');

        exit(RecordArg.FieldValue(StringArg.Value));
    end;
}