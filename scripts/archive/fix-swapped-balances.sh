#!/bin/sh

# fix-swapped-balances.sh -- patches bad combined fee transfer balances from genesis through
#                            state_hash 3NLw8PeQS7k67jHBQsZjWusVZ1PeU8nma2wf76ZKqsgxhQpH2zDu
#
# that's the first canonical block with a timestamp on April 7, 2021
# later that day, Mina v1.1.5 was released, containing a fix for the
# swapped combined fee transfer balances

# use the URI for your archive db
ARCHIVE_URI=${ARCHIVE_URI:-postgres://postgres@localhost:5432/archive}

SWAPPER=mina-swap-bad-balances

RUN_SWAP="$SWAPPER --archive-uri $ARCHIVE_URI"

$RUN_SWAP --state-hash 3NKjEfCzCZcfoakkg4i2iG9g2X3HLx2YuSiVCtNiNYRfzStTSRWr --sequence-no 3
$RUN_SWAP --state-hash 3NKjEfCzCZcfoakkg4i2iG9g2X3HLx2YuSiVCtNiNYRfzStTSRWr --sequence-no 4
$RUN_SWAP --state-hash 3NKjEfCzCZcfoakkg4i2iG9g2X3HLx2YuSiVCtNiNYRfzStTSRWr --sequence-no 5

$RUN_SWAP --state-hash 3NLVCk5bA1nCQLCqudxY8gq7TfASSxvuvwkgcAz1QCMjcvb5ijLE --sequence-no 3
$RUN_SWAP --state-hash 3NLVCk5bA1nCQLCqudxY8gq7TfASSxvuvwkgcAz1QCMjcvb5ijLE --sequence-no 4

$RUN_SWAP --state-hash 3NLBtAZnfZtrGyHsLpxzi9scUoPHgXCRNndEDvUMar42jrN5A3Vb --sequence-no 16
$RUN_SWAP --state-hash 3NLBtAZnfZtrGyHsLpxzi9scUoPHgXCRNndEDvUMar42jrN5A3Vb --sequence-no 17

$RUN_SWAP --state-hash 3NLJPo9fcJvmtYBhDS3bJNnvLkPokj8thU6bcAL36isADYtaHi3m --sequence-no 6
$RUN_SWAP --state-hash 3NLJPo9fcJvmtYBhDS3bJNnvLkPokj8thU6bcAL36isADYtaHi3m --sequence-no 7
$RUN_SWAP --state-hash 3NLJPo9fcJvmtYBhDS3bJNnvLkPokj8thU6bcAL36isADYtaHi3m --sequence-no 8

$RUN_SWAP --state-hash 3NKSXjinmeVEGMRGYz1S9UkmQAFD58awZgA2nZUm1moSsxkn4Txw --sequence-no 3
$RUN_SWAP --state-hash 3NKSXjinmeVEGMRGYz1S9UkmQAFD58awZgA2nZUm1moSsxkn4Txw --sequence-no 4

$RUN_SWAP --state-hash 3NKxVcd8DixEjjmuBb2k5xW5RTSMwhNSpRJNJ7KcKL7RW7jAVY2t --sequence-no 3
$RUN_SWAP --state-hash 3NKxVcd8DixEjjmuBb2k5xW5RTSMwhNSpRJNJ7KcKL7RW7jAVY2t --sequence-no 4

$RUN_SWAP --state-hash 3NKSoUrfAP9zqe6HP3EGWSrzhnpixbez7Hk7EerXjYpCybwKbdme  --sequence-no 3
$RUN_SWAP --state-hash 3NKSoUrfAP9zqe6HP3EGWSrzhnpixbez7Hk7EerXjYpCybwKbdme  --sequence-no 4

$RUN_SWAP --state-hash 3NKWgjnJukg2GaVRy66QUzm1kpKCH9fvBzcNkF56BEa6HHRjmfk7  --sequence-no 5

$RUN_SWAP --state-hash 3NLCEv7V6s6rgzwuqDYrdR7nWj1PXVc8sVMbLV6H9jBrcdMxsErM  --sequence-no 3

$RUN_SWAP --state-hash 3NKmkxwogfkV2x5oL4RuR3WFWmvNpKmFghdNSZfRiRHn8pjvCxJH  --sequence-no 4

$RUN_SWAP --state-hash 3NLkP7Wu2JovZj8MFch9ugfY6hGWwVe5Uy8J8qkeVVi16ay96dac  --sequence-no 9

$RUN_SWAP --state-hash 3NKeE2B3nh5p3YhNdqtbSADMj1AWR8Q7K3ufEvrJu8hs21JXb6An  --sequence-no 4

$RUN_SWAP --state-hash 3NKjsFwQDroJgKn2wHTLKhFqX3QxzbB9fmgMviz1rYbwSYdCN1Tv  --sequence-no 8

$RUN_SWAP --state-hash 3NKGnC2e3KxFh8yixHLt1vLUmf5yzdChQWVDLxfupgPcrPLC4Qso  --sequence-no 3

$RUN_SWAP --state-hash 3NKkT8BjXizuEuAeNPdFZf3tEkwFzjjYBj8CfJXgBrjDFLxsg4q5  --sequence-no 2

$RUN_SWAP --state-hash 3NLt3WEYazTo9XjKqCZjWRkozR7z6oz8dCBPuY2ixDKgNG2RctW7  --sequence-no 5

$RUN_SWAP --state-hash 3NL6aDHvieaB8z4STn8SbAzRy7896ubDxTWZ54f9MApiXCgGDDa1  --sequence-no 4

$RUN_SWAP --state-hash 3NKipHUjoHA1nCzpdqRoHrYTQjqCNAVVCUMtegWyk8Gykp9jMjo4  --sequence-no 15

$RUN_SWAP --state-hash 3NL1mqy56HS7E2nqEtgW6ZKmBFRGh8ks1YBjqR2jKtXvGtzzJgcM  --sequence-no 3

$RUN_SWAP --state-hash 3NLRt1K856f9nMUKjaWvWt3bZo5scLFk8Y8G1fpJqR5DRFt3toiM   --sequence-no 5

$RUN_SWAP --state-hash 3NKqsoFw6DY25SFpgZa8ivxaCi3HAzLF7pyHkR1WFNHQzB14ZTnx  --sequence-no 5

$RUN_SWAP --state-hash 3NL5AEXkAF2Pz4mzSRHNFgQdv1goUumr4g9TZNKyq6EY7Zv9rReS --sequence-no 3

$RUN_SWAP --state-hash 3NKQsA7of6rd5ZNUfGEUaZe48QFybj9LXEMpbhg6Li4ZPLm8V4jG  --sequence-no 3

$RUN_SWAP --state-hash 3NLgsc9n1b7scXwwjuoECrj6ZDYdvTQ8LEa1pjTWr8fvbzKGFtWa  --sequence-no 6

$RUN_SWAP --state-hash 3NLAhmQdt71XXxFWVoDLuGx3bDeSe3yTSayuTP1M3KayGujNnX7i  --sequence-no 2

$RUN_SWAP --state-hash 3NKvEDcmn25Nuv8RCtxaQ621bKyZZVK9W1JDNsyjvA4Hh8wnZFng  --sequence-no 5

$RUN_SWAP --state-hash 3NKyraYKU93pT7jMuk5K3AeoWbSXHYr383BH9DsE37ip2CEtVtyK  --sequence-no 4

$RUN_SWAP --state-hash 3NKki7THweuXrKyvAZ6NvvjwZsqmHEuXu5serfMaZqoNskusNkuX  --sequence-no 5

$RUN_SWAP --state-hash 3NKzQ7cA7G8MtRY8qbHaVbvD1bf5T1Kzb6nvbmvArQxjokM8i3r9  --sequence-no 15

$RUN_SWAP --state-hash 3NLJYpKcCtaHwaVcdR9GCvg1GCQAj3xecTm3fqSrcnnb4w2kBLkD  --sequence-no 11
