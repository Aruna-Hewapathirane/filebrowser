unit AudioPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

procedure PlayAudioWithMPV(const FilePath: string);
procedure StopAudio;

implementation

var
  CurrentProcess: TProcess = nil; // Keeps track of the currently playing process

procedure StopAudio;
begin
  if Assigned(CurrentProcess) then
  begin
    try
      // Terminate the process
      CurrentProcess.Terminate(0);
      CurrentProcess.Free;
      CurrentProcess := nil;
    except
      on E: Exception do
        WriteLn('Error while stopping audio: ', E.Message);
    end;
  end;
end;

procedure PlayAudioWithMPV(const FilePath: string);
begin
  // Stop the currently playing audio before starting a new one
  StopAudio;

  if not FileExists(FilePath) then
  begin
    WriteLn('Error: File not found - ', FilePath);
    Exit;
  end;

  CurrentProcess := TProcess.Create(nil);
  try
    // Set the mpv executable (ensure mpv is installed and in PATH)
    CurrentProcess.Executable := 'mpv';

    // Add the audio file as a parameter
    CurrentProcess.Parameters.Add(FilePath);

    // Optionally: Run mpv in the background (remove poWaitOnExit)
    CurrentProcess.Options := [poNoConsole];

    // Execute the mpv process
    CurrentProcess.Execute;

  except
    on E: Exception do
    begin
      WriteLn('Error while playing audio: ', E.Message);
      FreeAndNil(CurrentProcess); // Ensure no dangling process reference
    end;
  end;
end;

end.

