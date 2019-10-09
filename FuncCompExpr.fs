module FuncCompExpr

// Atomic operations
// * Create file
// * Delete file
// * Move file
// * Replace file
//
// Non-atomic operations
// * Read
// * Write
//
// What's the state if a crash happens during
// Create -> (1) Write -> Move -> (2) Read -> (3) Write -> Replace -> Delete?
// (1): File is created, but contents invalid. Rollback: Delete file
// (2): File is created, filled and moved. Rollback: Move back and delete
// (3): File is created, filled, moved, read (and acted upon?) but then contents become invalid. Rollback: (reverse acts?) moved back and delete
