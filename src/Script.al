page 66000 "sal3 Script"
{
    Caption = 'Script';
    PageType = Card;
    ApplicationArea = All;
    UsageCategory = Tasks;

    layout
    {
        area(Content)
        {
            group("General")
            {
                group("Script Group")
                {
                    Caption = 'Script';
                    field(Script; Script)
                    {
                        MultiLine = true;
                        ShowCaption = false;
                    }
                }
            }
        }
    }

    actions
    {
        area(Promoted)
        {
            actionref("Run Lexer Promoted"; "Run Lexer") { }
            actionref("Run Parser Promoted"; "Run Parser") { }
            actionref("Parse & Format Promoted"; "Parse & Format") { }
        }
        area(Processing)
        {
            action("Run Lexer")
            {
                Caption = 'Run Lexer';
                Image = Start;

                trigger OnAction()
                begin
                    RunLexer();
                end;
            }
            action("Run Parser")
            {
                Caption = 'Run Parser';
                Image = Start;

                trigger OnAction()
                begin
                    RunParser();
                end;
            }
            action("Parse & Format")
            {
                Caption = 'Parse & Format';
                Image = StyleSheet;

                trigger OnAction()
                begin
                    ParseAndFormat();
                end;
            }
        }
    }

    var
        Script: Text;

    local procedure RunLexer()
    var
        Lexer: Codeunit "sal3 Lexer";
        Lexemes: Codeunit "sal3 Lexemes";
        Lexeme: Interface "sal3 Lexeme";

        LexemesBuilder: TextBuilder;
    begin
        Lexer.Init(Script);

        while true do begin
            Lexeme := Lexer.Next();

            LexemesBuilder.AppendLine(Lexeme.ToString());

            if Lexeme is "sal3 Lexeme EOS" then
                break;
        end;

        Message(LexemesBuilder.ToText());
    end;

    local procedure RunParser()
    var
        Parser: Codeunit "sal3 Parser";
        Forms: List of [Interface "sal3 Form"];
        Form: Interface "sal3 Form";

        Builder: TextBuilder;
    begin
        Forms := Parser.Parse(Script);

        foreach Form in Forms do
            Builder.AppendLine(Form.ToString());

        Message(Builder.ToText());
    end;

    local procedure ParseAndFormat()
    var
        Parser: Codeunit "sal3 Parser";
        Formatter: Codeunit "sal3 Formatter";
        Forms: List of [Interface "sal3 Form"];
        Form: Interface "sal3 Form";

        Builder: TextBuilder;
    begin
        Forms := Parser.Parse(Script);

        foreach Form in Forms do begin
            Formatter.FormatForm(Form, Builder);
            Builder.AppendLine();
        end;

        Message(Builder.ToText());
    end;
}

