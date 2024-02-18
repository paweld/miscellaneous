unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LPDWeatherU;

type

  { TForm2 }

  TForm2 = class(TForm)
    bClose: TButton;
    FlowPanel1: TFlowPanel;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure GenForecastInfo;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

uses
  Unit1;

var
  wparr: Array of TPanel;

procedure TForm2.FormCreate(Sender: TObject);
begin
  GenForecastInfo;
end;

procedure TForm2.GenForecastInfo;
var
  i: Integer;

  function AddInfoPanel(weatheritem: TWeatherItem): TPanel;
  var
    img: TImage;
    l1, l2, l3, l4, l5, l6, l7: TLabel;
  begin
    Result := TPanel.Create(FlowPanel1);
    Result.Parent := FlowPanel1;
    Result.Width := 350;
    Result.Height := 150;
    Result.Color := clWhite;
    Result.BevelInner := bvNone;
    Result.BevelOuter := bvNone;
    Result.BorderStyle := bsSingle;
    Result.BorderSpacing.Around := 4;
    img := TImage.Create(Result);
    img.Parent := Result;
    img.Width := 100;
    img.Height := 100;
    img.Top := 25;
    img.Left := 10;
    try
      weatheritem.WeatherIcon.Position := 0;
      img.Picture.LoadFromStream(weatheritem.WeatherIcon);
    except
      img.Picture.Clear;
    end;
    l1 := TLabel.Create(Result);
    l1.Parent := Result;
    l1.Top := 20;
    l1.Left := 120;
    l1.Caption := 'City: ' + Form1.weather.City;
    l2 := TLabel.Create(Result);
    l2.Parent := Result;
    l2.Top := 35;
    l2.Left := 120;
    l2.Caption := 'Forecast time: ' + FormatDateTime('yyyy-mm-dd hh:nn', weatheritem.Date);
    l3 := TLabel.Create(Result);
    l3.Parent := Result;
    l3.Top := 50;
    l3.Left := 120;
    l3.Caption := 'Temp: ' + FormatFloat('0.00', weatheritem.Temp) + ' °C';
    l4 := TLabel.Create(Result);
    l4.Parent := Result;
    l4.Top := 65;
    l4.Left := 120;
    l4.Caption := 'Pressure: ' + FormatFloat('0', weatheritem.Pressure) + ' hPa';
    l5 := TLabel.Create(Result);
    l5.Parent := Result;
    l5.Top := 80;
    l5.Left := 120;
    l5.Caption := 'Humidity: ' + FormatFloat('0.00', weatheritem.Humidity) + ' %';
    l6 := TLabel.Create(Result);
    l6.Parent := Result;
    l6.Top := 95;
    l6.Left := 120;
    l6.Caption := 'Wind: ' + weatheritem.WindDirection + '; ' + FormatFloat('0.00', weatheritem.WindSpeed * 3.6) + ' km/h';
    l7 := TLabel.Create(Result);
    l7.Parent := Result;
    l7.Top := 110;
    l7.Left := 120;
    l7.Font.Style := [fsBold];
    l7.Font.Color := clNavy;
    l7.Caption := weatheritem.Description;
  end;

begin
  SetLength(wparr, Form1.weather.WeatherForecast.Count);
  for i := 0 to Form1.weather.WeatherForecast.Count - 1 do
    wparr[i] := AddInfoPanel(Form1.weather.WeatherForecast.Data[i]);
end;

end.

