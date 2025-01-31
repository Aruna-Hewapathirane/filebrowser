unit AudioPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

procedure PlayAudioWithMPV(const FilePath: string);

implementation

procedure PlayAudioWithMPV(const FilePath: string);
var
  Process: TProcess;
begin
  if not FileExists(FilePath) then
  begin
    WriteLn('Error: File not found - ', FilePath);
    Exit;
  end;

  Process := TProcess.Create(nil);
  try
    // Set the mpv executable (ensure mpv is installed and in PATH)
    Process.Executable := 'mpv';

    // Add the audio file as a parameter
    Process.Parameters.Add(FilePath);

    // Wait for mpv to finish before proceeding
    //Process.Options := [poWaitOnExit];

    // Execute the mpv process
    Process.Execute;

  except
    on E: Exception do
      WriteLn('Error while playing audio: ', E.Message);
  end;

  // Free the TProcess instance
  Process.Free;
end;

end.

