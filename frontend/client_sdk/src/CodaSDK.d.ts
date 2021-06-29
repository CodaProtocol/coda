import { uint32 as $$uint32 } from './TSTypes';
import { uint64 as $$uint64 } from './TSTypes';
export declare type publicKey = string;
export declare type privateKey = string;
export declare type uint64 = $$uint64;
export declare type uint32 = $$uint32;
export declare type keypair = {
    readonly privateKey: privateKey;
    readonly publicKey: publicKey;
};
export declare type signature = {
    readonly field: string;
    readonly scalar: string;
};
export declare type signed<signable> = {
    readonly publicKey: publicKey;
    readonly signature: signature;
    readonly payload: signable;
};
export declare type stakeDelegation = {
    readonly to: publicKey;
    readonly from: publicKey;
    readonly fee: uint64;
    readonly nonce: uint32;
    readonly memo?: string;
    readonly validUntil?: uint32;
};
export declare type payment = {
    readonly to: publicKey;
    readonly from: publicKey;
    readonly fee: uint64;
    readonly amount: uint64;
    readonly nonce: uint32;
    readonly memo?: string;
    readonly validUntil?: uint32;
};
/**
  * Generates a public/private keypair
  */
export declare const genKeys: () => keypair;
/**
  * Derives the public key of the corresponding private key
  *
  * @param privateKey - The private key used to get the corresponding public key
  * @returns A public key
  */
export declare const derivePublicKey: (privateKey: privateKey) => publicKey;
/**
  * Verifies if a keypair is valid by checking if the public key can be derived from
  * the private key and additionally checking if we can use the private key to
  * sign a transaction. If the keypair is invalid, an exception is thrown.
  *
  * @param keypair - A keypair
  * @returns True if the `keypair` is a verifiable keypair, otherwise throw an exception
   */
export declare const verifyKeypair: (keypair: keypair) => boolean;
/**
  * Signs an arbitrary message
  *
  * @param key - The keypair used to sign the message
  * @param message - An arbitrary string message to be signed
  * @returns A signed message
  */
export declare const signMessage: (message: string, key: keypair) => signed<string>;
/**
  * Verifies that a signature matches a message.
  *
  * @param signedMessage - A signed message
  * @returns True if the `signedMessage` contains a valid signature matching
  * the message and publicKey.
  */
export declare const verifyMessage: (signedMessage: signed<string>) => boolean;
/**
  * Signs a payment transaction using a private key.
  *
  * This type of transaction allows a user to transfer funds from one account
  * to another over the network.
  *
  * @param payment - An object describing the payment
  * @param key - The keypair used to sign the transaction
  * @returns A signed payment transaction
  */
export declare const signPayment: (payment: payment, key: keypair) => signed<payment>;
/**
  * Signs a stake delegation transaction using a private key.
  *
  * This type of transaction allows a user to delegate their
  * funds from one account to another for use in staking. The
  * account that is delegated to is then considered as having these
  * funds when determining whether it can produce a block in a given slot.
  *
  * @param stakeDelegation - An object describing the stake delegation
  * @param key - The keypair used to sign the transaction
  * @returns A signed stake delegation
  */
export declare const signStakeDelegation: (stakeDelegation: stakeDelegation, key: keypair) => signed<stakeDelegation>;
/**
  * Verifies a signed payment.
  *
  * @param signedPayment - A signed payment transaction
  * @returns True if the `signed(payment)` is a verifiable payment
   */
export declare const verifyPaymentSignature: (signedPayment: signed<payment>) => boolean;
/**
  * Verifies a signed stake delegation.
  *
  * @param signedStakeDelegation - A signed stake delegation
  * @returns True if the `signed(stakeDelegation)` is a verifiable stake delegation
   */
export declare const verifyStakeDelegationSignature: (signedStakeDelegation: signed<stakeDelegation>) => boolean;
/**
  * Converts a Rosetta signed transaction to a JSON string that is
  * compatible with GraphQL. The JSON string is a representation of
  * a `Signed_command` which is what our GraphQL expects.
  *
  * @param signedRosettaTxn - A signed Rosetta transaction
  * @returns A string that represents the JSON conversion of a signed Rosetta transaction`.
   */
export declare const signedRosettaTransactionToSignedCommand: (signedRosettaTxn: string) => string;
