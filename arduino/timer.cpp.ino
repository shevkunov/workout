#include <Time.h>  
#include <Wire.h>  
#include <DS1307RTC.h>
#include <time.h>
#include <avr/eeprom.h>

const int MOS_PIN = 2;
uint8_t* DAYS_OFFSET = reinterpret_cast<uint8_t*>(1);

time_t convertToTime(String calTimestamp) {
  // Like 20180810T143000Z
  struct tm tm;
  Serial.println("Parsing :" + calTimestamp);
  String year = calTimestamp.substring(0, 4);
  String month = calTimestamp.substring(4, 6);
  if (month.startsWith("0")) {
    month = month.substring(1);
  }
  String day = calTimestamp.substring(6, 8);
  if (day.startsWith("0")) {
    month = day.substring(1);
  }
  tm.tm_year = year.toInt();
  tm.tm_mon = month.toInt();
  tm.tm_mday = day.toInt();
  tm.tm_hour = calTimestamp.substring(9, 11).toInt();
  tm.tm_min = calTimestamp.substring(11, 13).toInt();
  tm.tm_sec = calTimestamp.substring(13, 15).toInt();

  time_t offset = time_t(60) * (60 * (24 * (365 * time_t(35) + 139) - 5) - 24) - 16;
  return mktime(&tm) + offset;
}

void setup()  {
  pinMode(MOS_PIN, OUTPUT);
  
  Serial.begin(9600);
  while (!Serial) ; // wait until Arduino Serial Monitor opens

  setSyncProvider(RTC.get);   // the function to get the time from the RTC
  if(timeStatus()!= timeSet) 
     Serial.println("Unable to sync with the RTC");
  else
     Serial.println("RTC has set the system time");      
}

void pump(const int t) {
  digitalWrite(MOS_PIN, HIGH);
  delay(t);
  digitalWrite(MOS_PIN, LOW);
}

const byte PUMP_HOUR = 16;
const byte PUMP_MIN = 51;
const byte PUMP_SEC_WIN = 5;
const int PUMP_DUR = 1500;

bool checkPumpIsNeeded() {
  const byte len = eeprom_read_byte(DAYS_OFFSET);
  const byte today = day();

  for (size_t i = 1; i <= len; ++i) {
    const bool pumpToday = (today == eeprom_read_byte(DAYS_OFFSET + i));
    const bool inWindow = (hour() == PUMP_HOUR) && (minute() == PUMP_MIN) && (second() < PUMP_SEC_WIN);
    if (pumpToday && inWindow) {
      Serial.println("Avaiting for pumping...");
      delay(PUMP_SEC_WIN * 1000); // passing through the window
      return true;
    }
  }

  return false;
}

void loop()
{
  time_t lastPrint = 0;
  while(true) {
   
    if (Serial.available()) {
      const String s = Serial.readString();
      const String cmd = s.substring(0, 2);
      
      if (cmd == "ST") {
        time_t newTime = convertToTime(s.substring(3, -1));
        RTC.set(newTime);
        setTime(newTime);
      } else if (cmd == "BP") {
        const int del = s.substring(3, -1).toInt();
        pump(del);
      } else if (cmd == "SD") {
        // set days
        // SD 01 02 10 - set array [01, 02, 10]
        size_t l = 3;
        size_t r;
        uint8_t i = 0;

        while (l < s.length()) {
          r = l;

          while (r < s.length() && s[r] != ' ') {
            ++r;
          }
          
          ++i;
          const uint8_t day = s.substring(l, r).toInt();
          Serial.println(day);
          eeprom_write_byte(DAYS_OFFSET + i, day);

          l = r + 1;
        }

        eeprom_write_byte(DAYS_OFFSET, i);

      } else if (cmd == "GD") {
        // get days
        const byte len = eeprom_read_byte(DAYS_OFFSET);

        Serial.print("Len:");
        Serial.print(len);
        for (size_t i = 0; i < len; ++i) {
          Serial.print(" ");
          Serial.print(eeprom_read_byte(DAYS_OFFSET + 1 + i));
        }
        Serial.println();
      } else {
        Serial.println("WC");
      }
    }

    if (checkPumpIsNeeded()) {
      Serial.println("Pumping...");
      pump(PUMP_DUR);
    }
    
    if (timeStatus() == timeSet) {
      digitalClockDisplay();
    } else {
      Serial.println("The time has not been set.");
      delay(4000);
    }
    delay(1000);
  }
}

void digitalClockDisplay(){
  // digital clock display of the time
  Serial.print(hour());
  printDigits(minute());
  printDigits(second());
  Serial.print(" ");
  Serial.print(day());
  Serial.print(" ");
  Serial.print(month());
  Serial.print(" ");
  Serial.print(year()); 
  Serial.println(); 
}

void printDigits(int digits){
  // utility function for digital clock display: prints preceding colon and leading 0
  Serial.print(":");
  if(digits < 10)
    Serial.print('0');
  Serial.print(digits);
}
