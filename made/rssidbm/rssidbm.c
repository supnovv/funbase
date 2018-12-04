#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "lnlylib.h"

#define GAIN_TBL_SIZE 16
static int8_t RF_RX_HWGAIN_TBL[GAIN_TBL_SIZE][3] = {
    [0][0] =  (-21), [0][1] = (-27), [0][2] = (-19),
    [1][0] =  (-18), [1][1] = (-24), [1][2] = (-13),
    [2][0] =  (-15), [2][1] = (-21), [2][2] = (-10),
    [3][0] =  (-12), [3][1] = (-18), [3][2] = (-10),
    [4][0] =  (-9),  [4][1] = (-15), [4][2] = (-10),
    [5][0] =  (-6),  [5][1] = (-12), [5][2] = (-10),
    [6][0] =  (-3),  [6][1] = (-9),  [6][2] = (-10),
    [7][0] =  0,     [7][1] = (-6),  [7][2] = 0,
    [8][0] =  0x7f,  [8][1] = (-3),  [8][2] = 0x7f,
    [9][0] =  0x7f , [9][1] = 0 ,    [9][2] = 0x7f,
    [10][0] = 0x7f, [10][1] = 0x7f, [10][2] = 0x7f,
    [11][0] = 0x7f, [11][1] = 0x7f, [11][2] = 0x7f,
    [12][0] = 0x7f, [12][1] = 0x7f, [12][2] = 0x7f,
    [13][0] = 0x7f, [13][1] = 0x7f, [13][2] = 0x7f,
    [14][0] = 0x7f, [14][1] = 0x7f, [14][2] = 0x7f
};

static int8_t
rf_rssi_convert(uint32_t rssi_reg)
{
    int8_t RssidBm = 0, GRx;
    int16_t PowerModem;
    PowerModem = (rssi_reg & 0xFC00) >> 10;
    GRx = 72+RF_RX_HWGAIN_TBL[rssi_reg & 0x7][0]+RF_RX_HWGAIN_TBL[(rssi_reg>>3) & 0xf][1]+RF_RX_HWGAIN_TBL[(rssi_reg>> 7)& 0x7][2];
    RssidBm=PowerModem-GRx;
    return (RssidBm);
}

static uint32_t
get_raw_rssi(uint32_t bt_rxchass, uint32_t bt_rxhwagc) {
  return ((bt_rxchass & 0xFF) << 10) | bt_rxhwagc;
}

static int8_t
get_dbm_rssi(uint32_t bt_rxchass, uint32_t bt_rxhwagc) {
    uint32_t raw_rssi = get_raw_rssi(bt_rxchass, bt_rxhwagc);
    return rf_rssi_convert(raw_rssi);
}

int main(int argc, char* argv[]) {
  if (argc != 2 && argc != 3) {
    printf("invalid arguments.\n");
    return 0;
  }
  if (argc == 2) {
    int rssi = 0;
    uint8_t rssidbm = 0;
    if (sscanf(argv[1], " %x", &rssi) != 1) {
      printf("invalid rssi.\n");
      return 0;
    }
    rssidbm = rf_rssi_convert(rssi);
    printf("rssi %x (%d) dbm %x (%d)\n", rssi, rssi, rssidbm, (int8_t)rssidbm);
    return 0;
  } else {
    if (strcmp(argv[1], "-f") != 0) {
      printf("invalid argument %s.\n", argv[1]);
      return 0;
    }
    l_file file = l_file_open_read(argv[2]);
    if (l_file_nt_open(&file)) {
      printf("cannot open file %s.\n", argv[2]);
      return 0;
    }
    l_byte line[64+1];
    l_umedit rxchass = 0;
    l_umedit rxhwagc = 0;
    l_umedit raw_rssi = 0;
    l_byte dbm_rssi = 0;
    while (l_file_read_line(&file, line, 64)) {
      if (sscanf((char*)line, " %x %x", &rxchass, &rxhwagc) != 2) {
        printf("cannot read rxchass rxhwagc in the line %s.\n", line);
	return 0;
      }
      raw_rssi = get_raw_rssi(rxchass, rxhwagc);
      dbm_rssi = get_dbm_rssi(rxchass, rxhwagc);
      printf("rxchass 0x%04x rxhwagc 0x%02x rssi 0x%02x (%d) dbm 0x%02x (%d)\n", rxchass, rxhwagc,
        raw_rssi, raw_rssi, dbm_rssi, (int8_t)dbm_rssi);
    }
    return 0;
  }
}
