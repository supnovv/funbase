#include <stdio.h>
#include <string.h>
#include "lnlylib.h"

#define L_TRADE_FLAG_BUY 0x01
#define L_TRADE_FLAG_DT 0x02
#define L_TRADE_FLAG_VR 0x04
#define L_TRADE_FLAG_VT 0x08
#define L_TRADE_FLAG_CB 0x10

typedef struct {
  char date[16];
  double price;
  int flags;
  int shares;
  double fee_rate;
} l_trade_item;

static void
l_trade_item_reset(l_trade_item* item)
{
  item->date[0] = 0;
  item->price = 0;
  item->flags = 0;
  item->shares = 0;
  item->fee_rate = 0;
}

static l_bool
l_read_a_trade_item(const l_byte* line, l_trade_item* item)
{
  l_byte buy_or_sell = 0;
  l_byte trade_type[4] = {0};
  int n = 0;
  
  /* date     price buy-sell shares fee-rate trade-type cost     cost-price total-shares total-cost market-value dest-value
     20180119 0.999 B        30000  3        VR         29978.99 (0.999)    30000        29978.99   29970.00     31000 */

  n = sscanf(line, " %8s %f %c %d %f %2s", item->date, &item->price, &buy_or_sell, &item->shares, &item->fee_rate, trade_type);

  if (n != 6) {
    goto read_fail;
  }

  if (strlen(item->date) != 8) {
    goto read_fail;
  }

  if (buy_or_sell != 'B' && buy_or_sell != 'S') {
    goto read_fail;
  }

  item->flags = (buy_or_sell == 'B' ? L_TRADE_FLAG_BUY : 0);

  if (strlen(trade_type) != 2) {
    goto read_fail;
  }

  if (trade_type[0] == 'D' && trade_type[1] == 'T') {
    item->flags |= L_TRADE_FLAG_DT;
  } else if (trade_type[0] == 'V' && trade_type[1] == 'R') {
    item->flags |= L_TRADE_FLAG_VR;
  } else if (trade_type[0] == 'V' && trade_type[1] == 'T') {
    item->flags |= L_TRADE_FLAG_VT;
  } else if (trade_type[0] == 'C' && trade_type[1] == 'B') {
    item->flags |= L-TRADE_FLAG_CB;
  } else {
    goto read_fail;
  }
  
  return true;

read_fail:
  l_trade_item_reset(item);
  return false;
}

typedef struct {
  l_trade_item trade;
  double cost;
  l_int total_shares;
  double total_cost;
  double cost_price;
  double market_value;
  double dest_value;
} l_eft_trade;
  
typedef struct {  
  l_byte* file;
  l_int* line_end;
  l_int eidx[1024];
} l_file_lines;

