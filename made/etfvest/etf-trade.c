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

  n = sscanf(line,  "%8s %lf %c %d %lf %2s", item->date, &item->price, &buy_or_sell, &item->shares, &item->fee_rate, trade_type);

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
  l_int total_shares;
  double total_cost;
  double market_value;
  double dest_value;
} l_trade_sum;

static l_bool
l_read_a_trade_sum(const l_byte* line, l_trade_sum* sum)
{
}

static l_bool
l_read_a_trade_item_and_sum(const l_byte* line, l_trade_item* item, l_trade_sum* sum)
{
}

static l_bool
l_process_etf_file(const l_byte* name)
{
  l_byte text[1024] = {0};
  l_trade_item item_0, item_1, item_2, item_3, item_4, item_5;
  l_trade_sum sum_0, sum_1, sum_2, sum_3, sum4, sum5;
  l_int cur_items = 0;
  l_int max_items = 5;
  l_file file;

  file = l_file_open_read_append(name);
  if (l_file_nt_open(&file)) {
    return false;
  }

  /* date     price buy-sell shares fee-rate trade-type cost     cost-price total-shares total-cost market-value dest-value
     20180119 0.999 B        30000  3        VR         29978.99 (0.999)    30000        29978.99   29970.00     31000 */
  // print "add <date> <price> <buy-sell> <shares> <fee-rate> <trade-type> - add 20180119 0.999 B 30000 3 VR\n"
  // print "del <n>\n"
  // print "commit\n"

  if (!l_file_read_line(&file, text, 1024)) {
    /* 1st time file read - read nothing out */
    l_trade_sum_reset(&prev_sum);
  } else {
    /* there is somthing, read until nothing can be read */
    while (l_file_read_line(&file, text, 1024)) {
      /* print out the line */
    }
    if (!l_read_a_trade_sum(text, &prev_sum)) {
     goto close_and_fail;
    }
  }

  
close_and_fail:
  l_file_close(&file);
  return false;
}

