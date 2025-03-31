# Registration Smart Contract

## Overview
The `registration` smart contract enables users to register, update their profiles, receive reviews, and verify their skills. It also allows the contract owner to assign skill verifiers who can confirm users' claimed skills.

## Features
- **User Registration:** Allows users to create a profile with a name, skills, and portfolio.
- **Profile Update:** Users can modify their profile details.
- **Reviews & Ratings:** Other users can review and rate registered users.
- **Skill Verification:** Verifiers can confirm users' skills, improving trust.
- **Skill Verifier Management:** The contract owner can assign verifiers for specific skills.
- **Read-Only Functions:** Retrieve user profiles and skill verifiers.

## Data Structures
- **Users (`users` map):** Stores user profiles including name, skills, portfolio, reviews, and verified skills.
- **Skill Verifiers (`skill-verifiers` map):** Stores lists of verifiers for each skill.

## Public Functions
### `register-user(name)`
Registers a new user with the given name.

### `update-profile(name, skills, portfolio)`
Allows a user to update their profile information.

### `add-review(user, rating, comment)`
Adds a review for a specified user with a rating and comment.

### `verify-skill(user, skill)`
Allows an authorized verifier to verify a user's skill.

### `add-skill-verifier(skill, verifier)`
Only the contract owner can add a verifier for a specific skill.

## Read-Only Functions
### `get-user-profile(user)`
Retrieves the profile details of a given user.

### `get-skill-verifiers(skill)`
Returns the list of verifiers assigned to a given skill.

## Error Handling
- `u100`: Not authorized.
- `u101`: User already registered.
- `u102`: User not found.
- `u103`: Maximum reviews reached.
- `u104`: Maximum verified skills reached.
- `u105`: Maximum verifiers reached.

## Access Control
- Only the contract owner can assign skill verifiers.
- Only designated verifiers can verify a user’s skill.

