from payouts_calculate import main as calculate_main
from payouts_calculate import read_staking_json_list
from payouts_validate import main as v_main
from payouts_validate import determine_slot_range_for_validation

if __name__ == "__main__":
    staking_ledger_available = read_staking_json_list()
    end=0
    for count in range(0, 7):
        calculate_main(count, False)
        v_main(count, True)
