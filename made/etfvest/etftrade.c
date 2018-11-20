#include <stdio.h>
#include <string.h>
#include <math.h>
#include "lnlylib.h"

#define L_TRADE_FLAG_BUY 0x01
#define L_TRADE_FLAG_DT 0x02
#define L_TRADE_FLAG_VR 0x04
#define L_TRADE_FLAG_VT 0x08
#define L_TRADE_FLAG_CB 0x10
#define L_TRADE_FLAG_BO 0x20
#define L_TRADE_FLAG_BI 0x40

typedef struct {
  char date[16];
  int date_n;
  double price;
  int flags;
  int shares;
  double fee_rate;
  char strategy[16];
  double bonus;
} l_etf_trade;

typedef struct {
  l_etf_trade trade;
  double trade_cost;
  int total_shares;
  double total_cost;
  double cost_price;
  double market_value;
  double dest_value;
} l_trade_entry;

static void
l_etf_trade_reset(l_etf_trade* trade)
{
  trade->date[0] = 0;
  trade->date_n = 0;
  trade->price = 0;
  trade->flags = 0;
  trade->shares = 0;
  trade->fee_rate = 0;
  trade->strategy[0] = 0;
  trade->bonus = 0;
}

static void
l_trade_entry_reset(l_trade_entry* entry)
{
  l_etf_trade_reset(&entry->trade);
  entry->trade_cost = 0;
  entry->total_shares = 0;
  entry->total_cost = 0;
  entry->cost_price = 0;
  entry->market_value = 0;
  entry->dest_value = 0;
}

static double
l_double_fractional_part(double a)
{
  double int_part = 0;
  return modf(a, &int_part);
}

static double
l_double_integral_part(double a)
{
  double int_part = 0;
  modf(a, &int_part);
  return int_part;
}

typedef struct {
  double i;
  double f;
} l_double_parts;

static l_double_parts
l_double_split(double a)
{
  l_double_parts parts;
  parts.f = modf(a, &parts.i);
  return parts;
}

static double
l_double_round(double a, l_byte fractional_part_digits)
{
  double enlarge_times = pow(10, fractional_part_digits);
  return round(a * enlarge_times) / enlarge_times;
}

static l_bool
l_trade_entry_build(l_trade_entry* curr, l_trade_entry* prev)
{
  l_etf_trade* trade = &curr->trade;
  double trade_value = trade->price * trade->shares;
  double trade_fee = l_double_round(trade_value * trade->fee_rate, 2);

  if (trade->flags & L_TRADE_FLAG_BUY) {
    curr->trade_cost = trade_value + trade_fee;
    curr->total_shares = prev->total_shares + trade->shares;
    curr->total_cost = prev->total_cost + curr->trade_cost;
  } else {
    curr->trade_cost = trade_value;
    curr->total_shares = prev->total_shares - trade->shares;
    if (curr->total_shares < 0) return false;
    curr->total_cost = prev->total_cost - curr->trade_cost + trade_fee;
    if (curr->total_cost < 0) return false;
  }

  if (trade->flags & L_TRADE_FLAG_BO) {
    curr->total_cost -= trade->bonus;
  }

  curr->cost_price = l_double_round(curr->total_cost / curr->total_shares, 3);
  curr->market_value = curr->total_shares * trade->price;

  if (trade->flags & L_TRADE_FLAG_DT) {
    curr->dest_value = prev->dest_value + curr->trade_cost;
  } else if (trade->flags & L_TRADE_FLAG_VR) {
    curr->dest_value = l_double_round(curr->market_value * 1.03, 2);
  } else if (trade->flags & L_TRADE_FLAG_VT) {
    curr->dest_value = (curr->market_value > prev->dest_value ? curr->market_value : prev->dest_value);
  } else if (trade->flags & L_TRADE_FLAG_CB) {
    curr->dest_value = (curr->market_value > prev->total_cost ? curr->market_value : prev->total_cost);
  } else if (trade->flags & L_TRADE_FLAG_BO) {
    curr->dest_value = prev->dest_value - trade->bonus;
  } else if (trade->flags & L_TRADE_FLAG_BI) {
    curr->dest_value = prev->dest_value + curr->trade_cost;
  } else {
    return false;
  }

  return true;
}

static l_bool
l_read_etf_trade(const l_byte* stream, l_etf_trade* trade)
{
  l_byte buy_or_sell = 0;
  char* date_char = 0;
  int n = 0;

  /* date     price buy-sell shares fee-rate strategy
     20180119 0.999 B        30000  3        VR */

  if (stream) {
    n = sscanf((const char*)stream, " %8s %lf %c %d %lf %15s", trade->date, &trade->price, &buy_or_sell, &trade->shares, &trade->fee_rate, trade->strategy);
  } else {
    n = scanf(" %8s %lf %c %d %lf %15s", trade->date, &trade->price, &buy_or_sell, &trade->shares, &trade->fee_rate, trade->strategy);
  }

  printf(" .. trade elements %d 6\n", n);
  if (n != 6) {
    goto read_fail;
  }

  printf(" .. trade date %s\n", trade->date);
  if (strlen(trade->date) != 8) {
    goto read_fail;
  }
  if (trade->date[0] <= '0' || trade->date[0] > '9') {
    goto read_fail;
  }
  date_char = trade->date + 1;
  while (*date_char) {
    if (*date_char < '0' || *date_char > '9') {
      goto read_fail;
    }
    date_char += 1;
  }
  if (sscanf(trade->date, "%d", &trade->date_n) != 1) {
    goto read_fail;
  }
  printf(" .. trade date n %d\n", trade->date_n);

  printf(" .. trade price %lf\n", trade->price);
  if (trade->price <= 0) {
    goto read_fail;
  }

  printf(" .. trade buy_or_sell %c\n", buy_or_sell);
  if (buy_or_sell != 'B' && buy_or_sell != 'S') {
    goto read_fail;
  }

  printf(" .. trade shares %d\n", trade->shares);
  if (trade->shares < 0 || (trade->shares % 100) != 0) {
    goto read_fail;
  }

  if (trade->fee_rate < 0) {
    goto read_fail;
  }

  trade->fee_rate *= 0.0001;

  printf(" .. trade fee_rate %lf\n", trade->fee_rate);

  trade->flags = (buy_or_sell == 'B' ? L_TRADE_FLAG_BUY : 0);

  printf(" .. trade strategy %s\n", trade->strategy);
  if (strlen(trade->strategy) < 2) {
    goto read_fail;
  }

  if (trade->strategy[0] == 'D' && trade->strategy[1] == 'T') {
    trade->flags |= L_TRADE_FLAG_DT;
    if ((trade->flags & L_TRADE_FLAG_BUY) == 0) {
      goto read_fail;
    }
  } else if (trade->strategy[0] == 'V' && trade->strategy[1] == 'R') {
    trade->flags |= L_TRADE_FLAG_VR;
  } else if (trade->strategy[0] == 'V' && trade->strategy[1] == 'T') {
    trade->flags |= L_TRADE_FLAG_VT;
  } else if (trade->strategy[0] == 'C' && trade->strategy[1] == 'B') {
    trade->flags |= L_TRADE_FLAG_CB;
    if ((trade->flags & L_TRADE_FLAG_BUY) == 0) {
      goto read_fail;
    }
  } else if (trade->strategy[0] == 'B' && trade->strategy[1] == 'O') {
    trade->flags |= L_TRADE_FLAG_BO;
    if (trade->flags & L_TRADE_FLAG_BUY) {
      goto read_fail;
    }
    if (sscanf(trade->strategy + 2, "%lf", &trade->bonus) != 1) {
      goto read_fail;
    }
    printf(" .. receive bonus %lf\n", trade->bonus);
    if (trade->bonus < 0) {
      goto read_fail;
    }
  } else if (trade->strategy[0] == 'B' && trade->strategy[1] == 'I') {
    trade->flags |= L_TRADE_FLAG_BI;
    if ((trade->flags & L_TRADE_FLAG_BUY) == 0) {
      goto read_fail;
    }
  } else {
    goto read_fail;
  }

  return true;

read_fail:
  l_etf_trade_reset(trade);
  return false;
}

static l_bool
l_read_trade_entry(const l_byte* stream, l_trade_entry* entry)
{
  if (!l_read_etf_trade(stream, &entry->trade)) {
    return false;
  } else {
    int n = 0;

    n = sscanf((const char*)stream, " %*s %*s %*s %*s %*s %*s %lf %d %lf %lf %lf %lf",
          &entry->trade_cost, &entry->total_shares, &entry->total_cost, &entry->cost_price, &entry->market_value, &entry->dest_value);

    printf(" .. trade entry elements %d 6\n", n);
    if (n != 6) {
      return false;
    }

    printf(" .. trade cost %lf\n", entry->trade_cost);
    if (entry->trade_cost < 0) {
      return false;
    }

    printf(" .. total shares %d\n", entry->total_shares);
    if (entry->total_shares < 0 || (entry->total_shares % 100) != 0) {
      return false;
    }

    printf(" .. total cost %lf\n", entry->total_cost);
    if (entry->total_cost == 0) {
      return false;
    }

    printf(" .. cost price %lf\n", entry->cost_price);
    printf(" .. market value %lf\n", entry->market_value);
    printf(" .. dest value %lf\n", entry->dest_value);

    if (entry->market_value < 0 || entry->dest_value < 0) {
      return false;
    }

    return true;
  }
}

static void
l_display_trade_entries(l_trade_entry* entry_arr, l_int index_s, l_int index_e)
{
  l_trade_entry* curr = entry_arr + index_s;
  l_trade_entry* entry_e = entry_arr + index_e;
  l_etf_trade* trade = 0;
  double earns = 0;

  /* date     price buy-sell shares fee-rate strategy cost     total-shares total-cost cost-price market-value dest-value
     20180119 0.999 B        30000  3        VR       29978.99 30000        29978.99   0.999      29970.00     31000 */

  printf("\nDATE     PRICE BUY-SELL SHARES FEE-RATE TRADE-STRATEGY TRADE-COST TOTAL-SHARES TOTAL-COST COST-PRICE MARKET-VALUE DEST-VALUE\n");

  for (; curr < entry_e; curr += 1) {
    trade = &curr->trade;
    earns = curr->market_value - curr->total_cost;
    printf("%8s %5.3lf %-8c %6d %8.1lf %-14s %10.2lf %12d %10.2lf %10.3lf %12.2lf %10.2lf %+8.3lf%% %+.2lf\n",
      trade->date, trade->price, (trade->flags & L_TRADE_FLAG_BUY) ? 'B' : 'S', trade->shares, trade->fee_rate * 10000, trade->strategy,
      curr->trade_cost, curr->total_shares, curr->total_cost, curr->cost_price, curr->market_value, curr->dest_value, earns * 100 / curr->total_cost, earns);
  }
}

static l_bool
l_commit_trade_entries(l_file* f, l_trade_entry* entry_arr, l_int index_s, l_int index_e)
{
  l_trade_entry* curr = entry_arr + index_s;
  l_trade_entry* entry_e = entry_arr + index_e;
  l_etf_trade* trade = 0;
  double earns = 0;
  int n = 0;

  for (; curr < entry_e; curr += 1) {
    trade = &curr->trade;
    earns = curr->market_value - curr->total_cost;
    n = fprintf((FILE*)f->file, "%8s %5.3lf %-8c %6d %8.1lf %-14s %10.2lf %12d %10.2lf %10.3lf %12.2lf %10.2lf %+8.3lf%% %+.2lf\n",
      trade->date, trade->price, (trade->flags & L_TRADE_FLAG_BUY) ? 'B' : 'S', trade->shares, trade->fee_rate * 10000, trade->strategy,
      curr->trade_cost, curr->total_shares, curr->total_cost, curr->cost_price, curr->market_value, curr->dest_value, earns * 100 / curr->total_cost, earns);
    if (n <= 0) {
      return false;
    }
  }

  return true;
}

#define L_PROGRAM "etftrade"
#define L_MAX_TRADE_ENTRIES 1024

static void
l_process_etf_file(const char* name, l_trade_entry* init_entry, l_file* file)
{
  l_trade_entry entry_arr[L_MAX_TRADE_ENTRIES + 1];
  l_trade_entry* cur_entry = 0;
  l_int curidx = 1;
  l_byte text_line[1024] = {0};
  char cmd[80] = {0};

  printf("\n" L_PROGRAM " %s commands:\n", name);
  printf(" add <date> <price> <buy-sell> <shares> <fee-rate> <strategy> - add 20180119 0.999 B 30000 3 VR\n");
  printf(" regen\n");
  printf(" del\n");
  printf(" commit\n");
  printf(" exit\n");

  entry_arr[0] = *init_entry;

  for (; ;) {
    printf("\n" L_PROGRAM " %s > ", name);
    if (scanf(" %8s", cmd) != 1) {
      printf("Read command failed.\n");
      return;
    }
    if (strcmp(cmd, "add") == 0) {
      if (curidx == L_MAX_TRADE_ENTRIES) {
        printf("Buffer is full, cannot add more items.\n");
      } else {
        cur_entry = entry_arr + curidx;
        if (l_read_etf_trade(0, &cur_entry->trade)) {
          if (l_trade_entry_build(cur_entry, cur_entry - 1)) {
            curidx += 1;
            l_display_trade_entries(entry_arr, 1, curidx);
          } else {
            printf("Build trade entry failed.\n");
          }
        } else {
          printf("Input trade information failed.\n");
        }
      }
    } else if (strcmp(cmd, "regen") == 0) {
      if (curidx > 1) {
        printf("Please delete new content first.\n");
      } else {
        curidx = 1;
        l_trade_entry_reset(&entry_arr[0]);
        if (!l_file_rewind(file)) {
          printf("Rewind file failed.\n");
          return;
        }
        while (l_file_read_line(file, text_line, 1024)) {
          printf(" .. get line %s\n", text_line);
          if (curidx == L_MAX_TRADE_ENTRIES) {
            printf("Buffer is full, cannot add more items %d.\n", (int)curidx);
            return;
          }
          cur_entry = entry_arr + curidx;
          if (!l_read_etf_trade(text_line, &cur_entry->trade)) {
            printf("Input trade information failed.\n");
            return;
          }
          if (!l_trade_entry_build(cur_entry, cur_entry - 1)) {
            printf("Build trade entry failed.\n");
            return;
          }
          curidx += 1;
        }
        l_display_trade_entries(entry_arr, 1, curidx);
        if (!l_file_rewind(file)) {
          printf("Rewind file before commit failed.\n");
          return;
        }
        l_commit_trade_entries(file, entry_arr, 1, curidx);
        return;
      }
    } else if (strcmp(cmd, "del") == 0) {
      if (curidx > 1) {
        curidx -= 1;
        if (curidx > 1) {
          l_display_trade_entries(entry_arr, 1, curidx);
        }
      } else {
        printf("There is no item to delete.\n");
      }
    } else if (strcmp(cmd, "commit") == 0) {
      if (curidx > 1) {
        if (l_commit_trade_entries(file, entry_arr, 1, curidx)) {
          curidx = 1;
        } else {
          printf("Commit failed.\n");
        }
      } else {
        printf("There is nothing need to commit.\n");
      }
    } else if (strcmp(cmd, "exit") == 0) {
      if (curidx > 1) {
        printf("Please commit the content before exit.\n");
      } else {
        return;
      }
    } else {
      printf("Invalid command '%s'.\n", cmd);
    }
  }
}

int main(int argc, char* argv[])
{
  l_byte text_line[1024];
  l_trade_entry init_entry;
  l_file file;

  lnlylib_setup(argc, argv);

  if (argc < 2) {
    printf("Usage: " L_PROGRAM " filename.etf\n");
    return 0;
  }

  file = l_file_open_read_write(argv[1]);
  if (l_file_nt_open(&file)) {
    printf("Cannot open file '%s'.\n", argv[1]);
    return 0;
  }

  if (!l_file_read_line(&file, text_line, 1024)) {
    /* the first line read failed */
    l_trade_entry_reset(&init_entry);
  } else {
    printf("\nDATE     PRICE BUY-SELL SHARES FEE-RATE TRADE-STRATEGY TRADE-COST TOTAL-SHARES TOTAL-COST COST-PRICE MARKET-VALUE DEST-VALUE\n");
    printf("%s", text_line);
    while (l_file_read_line(&file, text_line, 1024)) {
      printf("%s", text_line);
    }
    if (!l_read_trade_entry(text_line, &init_entry)) {
      printf("Read file failed '%s'.\n", argv[1]);
      goto return_label;
    }
  }

  l_process_etf_file(argv[1], &init_entry, &file);

return_label:
  l_file_close(&file);
  return 0;
}

