codeunit 66029 "sal3 Formatter"
{
    procedure FormatForm(Form: Interface "sal3 Form"; Builder: TextBuilder): Text
    var
        Cell: Interface "sal3 Form Cell";
        Number: Interface "sal3 Form Number";
        String: Interface "sal3 Form String";
        Symbol: Interface "sal3 Form Symbol";
    begin
        case true of
            Form is "sal3 Form Cell":
                begin
                    Cell := Form as "sal3 Form Cell";

                    Builder.Append('(');

                    FormatForm(Cell.Car, Builder);

                    while Cell.Cdr is "sal3 Form Cell" do begin
                        Cell := Cell.Cdr as "sal3 Form Cell";

                        Builder.Append(' ');
                        FormatForm(Cell.Car, Builder);
                    end;

                    if not (Cell.Cdr is "sal3 Form Nil") then begin
                        Builder.Append(' . ');
                        FormatForm(Cell.Cdr, Builder);
                    end;

                    Builder.Append(')');
                end;
            Form is "sal3 Form Number":
                begin
                    Number := Form as "sal3 Form Number";
                    Builder.Append(Format(Number.Value, 0, 9));
                end;
            Form is "sal3 Form String":
                begin
                    String := Form as "sal3 Form String";
                    Builder.Append('"');
                    Builder.Append(String.Value.Replace('"', '\"'));
                    Builder.Append('"');
                end;
            Form is "sal3 Form Symbol":
                begin
                    Symbol := Form as "sal3 Form Symbol";
                    Builder.Append(Symbol.Name);
                end;
            Form is "sal3 Form Nil":
                begin
                    Builder.Append('nil');
                end;
            else
                Error('Format not implemented for %1.', Form.ToString());
        end;
    end;
}