# MicroRealty: Micro Real Estate Fractionalization Platform

MicroRealty is a Clarity smart contract for the Stacks blockchain that enables the tokenization of small real estate properties, allowing for partial ownership and automated revenue distribution.

## Overview

The platform allows real estate owners to tokenize their properties by dividing them into shares that can be purchased by investors. Revenue generated from these properties is automatically distributed to shareholders proportional to their ownership stake.

## Features

- **Property Tokenization**: Real estate owners can create digital representations of their properties divided into shares.
- **Fractional Ownership**: Investors can purchase shares of properties, enabling micro-investments in real estate.
- **Automated Revenue Distribution**: Revenue from properties is automatically distributed to shareholders.
- **Active/Inactive Properties**: Contract owner can activate or deactivate properties as needed.
- **Security-focused Design**: Implementation includes comprehensive input validation and security measures.

## Contract Functions

### For Property Owners

- `create-property`: Create a new tokenized property (contract owner only).
- `add-revenue`: Add revenue to a property (contract owner or approved managers).
- `deactivate-property`: Temporarily disable a property (contract owner only).
- `reactivate-property`: Re-enable a previously deactivated property (contract owner only).

### For Investors

- `purchase-shares`: Buy shares of a property.
- `claim-revenue`: Claim your share of revenue from properties you've invested in.

### Read-only Functions

- `get-property`: Retrieve details about a specific property.
- `get-ownership`: Check ownership details for a specific property and owner.
- `get-property-revenue`: View revenue information for a property.
- `has-shares`: Check if a user owns shares in a property.
- `property-exists`: Verify if a property ID exists.
- `calculate-revenue-share`: Calculate a user's share of revenue for a property.

## Error Codes

The contract uses the following error codes:

- `err-owner-only (u100)`: Only the contract owner can perform this action.
- `err-property-exists (u101)`: The property already exists.
- `err-property-not-found (u102)`: The specified property does not exist.
- `err-insufficient-tokens (u103)`: Insufficient tokens/shares for the operation.
- `err-unauthorized (u104)`: The caller is not authorized to perform this action.
- `err-property-not-active (u105)`: The property is not currently active.
- `err-invalid-amount (u106)`: The specified amount is invalid.
- `err-null-string (u107)`: Empty string provided where a value is required.
- `err-invalid-property-id (u108)`: The property ID is invalid.

## Development

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) for local development and testing
- [Stacks Wallet](https://www.hiro.so/wallet) for interacting with the deployed contract

### Local Development

1. Clone the repository
2. Install Clarinet
3. Run tests with `clarinet test`

## Security

The contract implements several security features:

- Comprehensive input validation
- Property ID verification
- Proper authorization checks
- Protection against integer overflow and division by zero

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.