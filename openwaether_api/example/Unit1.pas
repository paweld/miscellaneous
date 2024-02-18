unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, CalendarLite, Types, DateUtils, LPDWeatherU;

type

  { TForm1 }

  TForm1 = class(TForm)
    bGet: TButton;
    Button1: TButton;
    CalendarLite1: TCalendarLite;
    cblang: TComboBox;
    ecity: TEdit;
    eapikey: TEdit;
    fselat: TFloatSpinEdit;
    fselon: TFloatSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rbcoord: TRadioButton;
    rbcity: TRadioButton;
    procedure bGetClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CalendarLite1DateChange(Sender: TObject);
    procedure CalendarLite1DrawCell(Sender: TObject; ACanvas: TCanvas; AYear, AMonth, ADay: Word; AState: TCalCellStates; var ARect: TRect;
      var AContinueDrawing: Boolean);
    procedure CalendarLite1Hint(Sender: TObject; AYear, AMonth, ADay: Word; var AText: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ClearInfo;
  public
    weather: TWeather;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  Unit2;

procedure TForm1.bGetClick(Sender: TObject);
var
  b: Boolean;
begin
  weather.APIKey := eapikey.Text;
  case cblang.ItemIndex of
    0: weather.Lang := wlEn;
    1: weather.Lang := wlEs;
    2: weather.Lang := wlPl;
    3: weather.Lang := wlUk;
    4: weather.Lang := wlZh_cn;
  end;
  if rbcity.Checked then
    b := weather.Get(eCity.Text)
  else
    b := weather.Get(fselat.Value, fselon.Value);
  if not b then
    ShowMessage(weather.LastError)
  else
  begin
    eCity.Text := weather.City;
    fselat.Value := weather.Latitude;
    fselon.Value := weather.Longitude;
    CalendarLite1DateChange(Sender);
    CalendarLite1.Invalidate;
    CalendarLite1.Date := Date;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Application.CreateForm(TForm2, Form2);
  Form2.Width := 750;
  Form2.Height := 400;
  Form2.Caption := 'Forecast';
  Form2.Position := poOwnerFormCenter;
  Form2.ShowModal;
  FreeAndNil(Form2);
end;

procedure TForm1.CalendarLite1DateChange(Sender: TObject);
var
  idx: Integer;
begin
  ClearInfo;
  Label5.Caption := 'Date: ' + FormatDateTime('yyyy-mm-dd', CalendarLite1.Date);
  idx := weather.WeatherForecast.IndexOfDate(IncHour(CalendarLite1.Date, HourOf(Now)));
  if CalendarLite1.Date = Date then //if today then current weather
  begin
    if weather.CurrentWeather.WeatherIcon.Size > 0 then
    begin
      weather.CurrentWeather.WeatherIcon.Position := 0;
      Image1.Picture.LoadFromStream(weather.CurrentWeather.WeatherIcon);
      Label6.Caption := 'Temp: ' + FormatFloat('0.00', weather.CurrentWeather.Temp) + ' °C -> Feels like: ' +
        FormatFloat('0.00', weather.CurrentWeather.TempFeelsLike) + ' °C';
      Label7.Caption := 'Pressure: ' + FormatFloat('0', weather.CurrentWeather.Pressure) + ' hPa';
      Label8.Caption := 'Humidity: ' + FormatFloat('0.0', weather.CurrentWeather.Humidity) + ' %';
      Label9.Caption := 'Wind: ' + weather.CurrentWeather.WindDirection + ' speed: ' +
        FormatFloat('0.00', weather.CurrentWeather.WindSpeed) + ' m/s';
      Label10.Caption := weather.CurrentWeather.Description;
    end;
  end
  else if idx >= 0 then //else if it is not today and there is a forecast for that day
  begin
    if weather.WeatherForecast.Data[idx].WeatherIcon.Size > 0 then
    begin
      weather.WeatherForecast.Data[idx].WeatherIcon.Position := 0;
      Image1.Picture.LoadFromStream(weather.WeatherForecast.Data[idx].WeatherIcon);
      Label6.Caption := 'Temp: ' + FormatFloat('0.00', weather.WeatherForecast.Data[idx].Temp) + ' °C -> Feels like: ' +
        FormatFloat('0.00', weather.WeatherForecast.Data[idx].TempFeelsLike) + ' °C';
      Label7.Caption := 'Pressure: ' + FormatFloat('0', weather.WeatherForecast.Data[idx].Pressure) + ' hPa';
      Label8.Caption := 'Humidity: ' + FormatFloat('0.0', weather.WeatherForecast.Data[idx].Humidity) + ' %';
      Label9.Caption := 'Wind: ' + weather.WeatherForecast.Data[idx].WindDirection + ' speed: ' +
        FormatFloat('0.00', weather.WeatherForecast.Data[idx].WindSpeed) + ' m/s';
      Label10.Caption := weather.WeatherForecast.Data[idx].Description;
    end;
  end;
end;

procedure TForm1.CalendarLite1DrawCell(Sender: TObject; ACanvas: TCanvas; AYear, AMonth, ADay: Word; AState: TCalCellStates; var ARect: TRect;
  var AContinueDrawing: Boolean);
var
  idx: Integer;
  png: Graphics.TPortableNetworkGraphic;
  r: TRect;
begin
  if csSelectedDay in AState then
    ACanvas.Brush.Color := CalendarLite1.Colors.SelectedDateColor
  else
    ACanvas.Brush.Color := CalendarLite1.Colors.BackgroundColor;
  ACanvas.FillRect(ARect);
  ACanvas.Brush.Style := bsClear;
  idx := weather.WeatherForecast.IndexOfDate(EncodeDateTime(AYear, AMonth, ADay, HourOf(Now), 0, 0, 0));
  if EncodeDate(AYear, AMonth, ADay) = Date then  //if today then current weather
  begin
    if weather.CurrentWeather.WeatherIcon.Size > 0 then
    begin
      png := Graphics.TPortableNetworkGraphic.Create;
      weather.CurrentWeather.WeatherIcon.Position := 0;
      png.LoadFromStream(weather.CurrentWeather.WeatherIcon);
      r.Left := ARect.Left + 2;
      r.Top := ARect.Top;
      r.Width := 25;
      r.Height := 25;
      ACanvas.StretchDraw(r, png);
      png.Free;
    end;
  end
  else if idx >= 0 then //else if it is not today and there is a forecast for that day
  begin
    if weather.WeatherForecast.Data[idx].WeatherIcon.Size > 0 then
    begin
      png := Graphics.TPortableNetworkGraphic.Create;
      weather.WeatherForecast.Data[idx].WeatherIcon.Position := 0;
      png.LoadFromStream(weather.WeatherForecast.Data[idx].WeatherIcon);
      r.Left := ARect.Left + 2;
      r.Top := ARect.Top;
      r.Width := 24;
      r.Height := 24;
      ACanvas.StretchDraw(r, png);
      png.Free;
    end;
  end;
end;

procedure TForm1.CalendarLite1Hint(Sender: TObject; AYear, AMonth, ADay: Word; var AText: String);
var
  idx: Integer;
begin
  idx := weather.WeatherForecast.IndexOfDate(EncodeDateTime(AYear, AMonth, ADay, HourOf(Now), 0, 0, 0));
  if idx >= 0 then
  begin
    AText := Format('Weather description: %s' + #13#10 +
      'Pressure: %3.0f hPa' + #13#10 +
      'Humidity: %3.1f%%' + #13#10 +
      'Temp: %3.2f °C',
      [weather.WeatherForecast.Data[idx].Description, weather.WeatherForecast.Data[idx].Pressure,
      weather.WeatherForecast.Data[idx].Humidity, weather.WeatherForecast.Data[idx].Temp]);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'OpenWeather API sample';
  Application.HintPause := 100;
  ClearInfo;
  weather := TWeather.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  weather.Free;
end;

procedure TForm1.ClearInfo;
begin
  Image1.Picture.Clear;
  Label5.Caption := '';
  Label6.Caption := '';
  Label7.Caption := '';
  Label8.Caption := '';
  Label9.Caption := '';
  Label10.Caption := '';
end;

end.

