# NFT-Backed Collateral Contract

## Overview
The **NFT-Backed Collateral Contract** allows users to deposit NFTs as collateral to receive loans in STX tokens. Users can repay their loans to reclaim their NFTs, while the contract owner has the authority to liquidate overdue loans and seize collateralized NFTs.

## Features
- **Deposit NFT as Collateral**: Users can lock an NFT in the contract to receive a loan.
- **Loan Repayment & Collateral Retrieval**: Borrowers can repay their loans to retrieve their NFTs.
- **Loan Liquidation**: The contract owner can liquidate overdue loans, seizing the NFT.
- **Read-Only Queries**: Users can check loan details and whether an NFT is collateralized.

## Contract Details

### Constants
- `contract-owner`: The owner of the contract (deployer).
- `err-not-owner (u100)`: Error when a non-owner attempts an unauthorized action.
- `err-already-collateralized (u101)`: Error when trying to collateralize an NFT that is already locked.
- `err-no-active-loan (u102)`: Error when trying to access a non-existent or settled loan.
- `err-insufficient-repayment (u103)`: Error when repayment amount is insufficient.

### Data Variables
- `next-loan-id`: Tracks the next available loan ID.

### Data Maps
- `loans`: Stores active loan details, indexed by loan ID.
- `collateralized-nfts`: Tracks collateralized NFTs, mapping NFT IDs to loan IDs.

### Token Definitions
- `collateral-nft`: A non-fungible token (NFT) that can be used as collateral.

## Public Functions

### `deposit-collateral-and-borrow(nft-id, loan-amount, repayment-amount, duration)`
- Locks an NFT as collateral.
- Issues a loan in STX tokens.
- Requires transfer of NFT to contract.
- Returns the assigned `loan-id`.

### `repay-loan-and-retrieve-collateral(loan-id)`
- Allows the borrower to repay the loan amount.
- Transfers the NFT back to the borrower.
- Deletes loan record upon successful repayment.

### `liquidate-overdue-loan(loan-id)`
- Allows the contract owner to liquidate overdue loans.
- Transfers the NFT to the contract owner upon liquidation.

## Read-Only Functions

### `get-loan-details(loan-id)`
- Returns details of a loan if it exists.

### `is-nft-collateralized(nft-id)`
- Returns `true` if the NFT is locked as collateral, otherwise `false`.

## Security Considerations
- Only the contract owner can liquidate overdue loans.
- Loans are linked to block height to determine expiration.
- Users must approve NFT transfers to the contract.

## Future Enhancements
- Support for multiple collateral types.
- Dynamic interest rates.
- Auction mechanism for liquidated NFTs.

This contract provides a decentralized way to leverage NFTs for liquidity while ensuring secure and fair handling of assets.