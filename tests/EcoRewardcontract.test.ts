import { describe, expect, it, beforeEach } from "vitest";
import { Cl, ClarityType } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;
const wallet3 = accounts.get("wallet_3")!;

const contractName = "EcoRewardcontract";

// Helper to create valid hashes (32 bytes = 64 hex chars)
const createHash = (seed: string) => {
  // Create a valid 64-character hex string from the seed
  const hash = seed.split('').map(c => c.charCodeAt(0).toString(16).padStart(2, '0')).join('');
  return Cl.bufferFromHex(hash.padEnd(64, 'a').slice(0, 64));
};

describe("EcoReward Contract - Security Tests", () => {
  
  describe("Initialization", () => {
    it("ensures contract is properly initialized", () => {
      const status = simnet.callReadOnlyFn(
        contractName,
        "get-contract-status",
        [],
        deployer
      );
      expect(status.result).toHaveClarityType(ClarityType.Tuple);
    });

    it("has correct token metadata", () => {
      const name = simnet.callReadOnlyFn(contractName, "get-name", [], deployer);
      expect(name.result).toBeOk(Cl.stringAscii("EcoToken"));

      const symbol = simnet.callReadOnlyFn(contractName, "get-symbol", [], deployer);
      expect(symbol.result).toBeOk(Cl.stringAscii("ECO"));

      const decimals = simnet.callReadOnlyFn(contractName, "get-decimals", [], deployer);
      expect(decimals.result).toBeOk(Cl.uint(6));
    });
  });

  describe("Action Submission Security", () => {
    it("allows valid action submission", () => {
      // Mine blocks to move past initial block height
      simnet.mineEmptyBlocks(15);
      
      const result = simnet.callPublicFn(
        contractName,
        "submit-action",
        [
          Cl.uint(1), // ACTION-CLEANUP
          createHash("location123"),
          createHash("proof456")
        ],
        wallet1
      );
      expect(result.result).toBeOk(Cl.uint(1));
    });

    it("prevents empty location hash", () => {
      const result = simnet.callPublicFn(
        contractName,
        "submit-action",
        [
          Cl.uint(1),
          Cl.bufferFromHex("0000000000000000000000000000000000000000000000000000000000000000"),
          createHash("proof456")
        ],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(114)); // ERR-INVALID-INPUT
    });

    it("prevents empty proof hash", () => {
      const result = simnet.callPublicFn(
        contractName,
        "submit-action",
        [
          Cl.uint(1),
          createHash("location123"),
          Cl.bufferFromHex("0000000000000000000000000000000000000000000000000000000000000000")
        ],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(114)); // ERR-INVALID-INPUT
    });

    it("prevents invalid action type", () => {
      const result = simnet.callPublicFn(
        contractName,
        "submit-action",
        [
          Cl.uint(99), // Invalid action type
          createHash("location123"),
          createHash("proof456")
        ],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(103)); // ERR-INVALID-ACTION
    });

    it("enforces rate limiting", () => {
      // Mine blocks to move past initial block height
      simnet.mineEmptyBlocks(15);
      
      // First submission should succeed
      const result1 = simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );
      expect(result1.result).toBeOk(Cl.uint(1));

      // Immediate second submission should fail (within 10 blocks)
      const result2 = simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc2"), createHash("proof2")],
        wallet1
      );
      expect(result2.result).toBeErr(Cl.uint(111)); // ERR-RATE-LIMIT-EXCEEDED
      
      // After mining 10 blocks, it should succeed
      simnet.mineEmptyBlocks(10);
      const result3 = simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc3"), createHash("proof3")],
        wallet1
      );
      expect(result3.result).toBeOk(Cl.uint(2));
    });
  });

  describe("Verification Security", () => {
    it("allows owner to verify actions", () => {
      // Mine blocks first
      simnet.mineEmptyBlocks(15);
      
      // Submit action
      const submitResult = simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );
      expect(submitResult.result).toBeOk(Cl.uint(1));

      // Verify as owner
      const result = simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        deployer
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents non-owner from verifying without authorization", () => {
      // Mine blocks first
      simnet.mineEmptyBlocks(15);
      
      // Submit action
      simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );

      // Try to verify as non-owner
      const result = simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        wallet2
      );
      expect(result.result).toBeErr(Cl.uint(113)); // ERR-UNAUTHORIZED
    });

    it("prevents double verification", () => {
      // Mine blocks first
      simnet.mineEmptyBlocks(15);
      
      // Submit and verify
      simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );
      simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        deployer
      );

      // Try to verify again
      const result = simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        deployer
      );
      expect(result.result).toBeErr(Cl.uint(104)); // ERR-ALREADY-VERIFIED
    });

    it("allows authorized verifiers to verify", () => {
      // Mine blocks first
      simnet.mineEmptyBlocks(15);
      
      // Add wallet2 as verifier
      simnet.callPublicFn(
        contractName,
        "add-verifier",
        [Cl.principal(wallet2)],
        deployer
      );

      // Submit action
      simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );

      // Verify as authorized verifier
      const result = simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        wallet2
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });
  });

  describe("Token Transfer Security", () => {
    it("allows valid token trades", () => {
      // Mine blocks first
      simnet.mineEmptyBlocks(15);
      
      // Setup: submit and verify action to get tokens
      simnet.callPublicFn(
        contractName,
        "submit-action",
        [Cl.uint(1), createHash("loc1"), createHash("proof1")],
        wallet1
      );
      simnet.callPublicFn(
        contractName,
        "verify-action",
        [Cl.principal(wallet1), Cl.uint(1)],
        deployer
      );

      // Trade tokens
      const result = simnet.callPublicFn(
        contractName,
        "trade-tokens",
        [Cl.uint(50), Cl.principal(wallet2)],
        wallet1
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents trading to self", () => {
      const result = simnet.callPublicFn(
        contractName,
        "trade-tokens",
        [Cl.uint(50), Cl.principal(wallet1)],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(114)); // ERR-INVALID-INPUT
    });

    it("prevents trading more than balance", () => {
      const result = simnet.callPublicFn(
        contractName,
        "trade-tokens",
        [Cl.uint(999999), Cl.principal(wallet2)],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(102)); // ERR-INSUFFICIENT-BALANCE
    });
  });

  describe("Sponsor Security", () => {
    it("allows sponsor registration with valid name", () => {
      const result = simnet.callPublicFn(
        contractName,
        "register-sponsor",
        [Cl.stringUtf8("EcoCorp")],
        wallet2
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents sponsor registration with empty name", () => {
      const result = simnet.callPublicFn(
        contractName,
        "register-sponsor",
        [Cl.stringUtf8("")],
        wallet2
      );
      expect(result.result).toBeErr(Cl.uint(114)); // ERR-INVALID-INPUT
    });

    it("allows sponsor contributions", () => {
      // Register sponsor
      simnet.callPublicFn(
        contractName,
        "register-sponsor",
        [Cl.stringUtf8("EcoCorp")],
        wallet2
      );

      // Contribute
      const result = simnet.callPublicFn(
        contractName,
        "sponsor-contribute",
        [Cl.uint(1000000)],
        wallet2
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("allows sponsor withdrawals", () => {
      // Register and contribute
      simnet.callPublicFn(
        contractName,
        "register-sponsor",
        [Cl.stringUtf8("EcoCorp")],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "sponsor-contribute",
        [Cl.uint(1000000)],
        wallet2
      );

      // Withdraw
      const result = simnet.callPublicFn(
        contractName,
        "sponsor-withdraw",
        [Cl.uint(500000)],
        wallet2
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents withdrawal more than available balance", () => {
      // Register and contribute
      simnet.callPublicFn(
        contractName,
        "register-sponsor",
        [Cl.stringUtf8("EcoCorp")],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "sponsor-contribute",
        [Cl.uint(1000)],
        wallet2
      );

      // Try to withdraw more
      const result = simnet.callPublicFn(
        contractName,
        "sponsor-withdraw",
        [Cl.uint(2000)],
        wallet2
      );
      expect(result.result).toBeErr(Cl.uint(107)); // ERR-INSUFFICIENT-SPONSOR-BALANCE
    });
  });

  describe("Access Control", () => {
    it("allows owner to add verifiers", () => {
      const result = simnet.callPublicFn(
        contractName,
        "add-verifier",
        [Cl.principal(wallet2)],
        deployer
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents non-owner from adding verifiers", () => {
      const result = simnet.callPublicFn(
        contractName,
        "add-verifier",
        [Cl.principal(wallet3)],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(100)); // ERR-OWNER-ONLY
    });

    it("allows owner to remove verifiers", () => {
      // Add then remove
      simnet.callPublicFn(
        contractName,
        "add-verifier",
        [Cl.principal(wallet2)],
        deployer
      );
      const result = simnet.callPublicFn(
        contractName,
        "remove-verifier",
        [Cl.principal(wallet2)],
        deployer
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("allows owner to toggle contract", () => {
      const result = simnet.callPublicFn(
        contractName,
        "toggle-contract",
        [Cl.bool(false)],
        deployer
      );
      expect(result.result).toBeOk(Cl.bool(true));
    });

    it("prevents non-owner from toggling contract", () => {
      const result = simnet.callPublicFn(
        contractName,
        "toggle-contract",
        [Cl.bool(false)],
        wallet1
      );
      expect(result.result).toBeErr(Cl.uint(100)); // ERR-OWNER-ONLY
    });
  });

  describe("Read-Only Functions", () => {
    it("returns correct contract status", () => {
      const result = simnet.callReadOnlyFn(
        contractName,
        "get-contract-status",
        [],
        deployer
      );
      expect(result.result).toHaveClarityType(ClarityType.Tuple);
    });

    it("checks verifier authorization correctly", () => {
      const result1 = simnet.callReadOnlyFn(
        contractName,
        "is-authorized-verifier",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(result1.result).toBeBool(false);

      // Add verifier
      simnet.callPublicFn(
        contractName,
        "add-verifier",
        [Cl.principal(wallet1)],
        deployer
      );

      const result2 = simnet.callReadOnlyFn(
        contractName,
        "is-authorized-verifier",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(result2.result).toBeBool(true);
    });

    it("returns user statistics", () => {
      const result = simnet.callReadOnlyFn(
        contractName,
        "get-user-stats",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(result.result).toHaveClarityType(ClarityType.Tuple);
    });
  });
});
