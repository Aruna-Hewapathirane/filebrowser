unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,
  ExtCtrls, ComCtrls, AudioPlayer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    ShellListView1: TShellListView;
    ShellTreeView1: TShellTreeView;
    procedure ShellListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    procedure ShowFilePreview(FilePath: String; FileExt: String);
    procedure ShowTextFile(FilePath: String);
    procedure ShowImageFile(FilePath: String);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ShellListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  FilePath: String;
  FileExt: String;
begin
  // Ensure an item is selected in the ShellListView
  if Assigned(Item) then
  begin
    FilePath := ShellTreeView1.Path + PathDelim + ShellListView1.Items[Item.Index].Caption;
    writeln('SHellListView: ', FilePath);
    FileExt := LowerCase(ExtractFileExt(FilePath));  // Get the file extension

    // Call the procedure to display the file preview
    ShowFilePreview(FilePath,FileExt);
  end;
end;


procedure TForm1.ShowTextFile(FilePath: String);
begin
  try
    // Load and show the contents of the text file in Memo1
    Memo1.Lines.LoadFromFile(FilePath);
  except
    on E: Exception do
      ShowMessage('Error loading text file: ' + E.Message);
  end;
end;

procedure TForm1.ShowImageFile(FilePath: String);
begin
  try
    // Load and show the image in Image1
    Image1.Picture.LoadFromFile(FilePath);
  except
    on E: Exception do
      ShowMessage('Error loading image file: ' + E.Message);
  end;
end;



procedure TForm1.ShowFilePreview(FilePath: String; FileExt: String);
begin
  // Clear previous previews
  Memo1.Clear;
  Image1.Picture := nil;

  // Check the file extension and decide how to display it
  case FileExt of
    '.txt', '.c', '.cfg', '.py', '.php', '.html', '.css', '.js', '.log', '.xml', '.json':
      // Display text files (including PHP, HTML, JS, etc.) in Memo
      ShowTextFile(FilePath);

    '.jpg', '.jpeg', '.png', '.gif', '.bmp', '.tiff':
      // Display image files in Image
      ShowImageFile(FilePath);

    '.pdf':
      // Example for PDF files, could use external viewer
      ShowMessage('Preview for PDF files is not implemented yet.');

    '.mp3', '.wav':
    // Play an audio file
    PlayAudioWithMPV(Filepath);

    else
      ShowMessage('Unsupported file type.');
  end;
end;


end.

