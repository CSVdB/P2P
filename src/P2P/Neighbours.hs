module P2P.Neighbours where

import P2P.Chan
import P2P.SockAddr

updateNeighbours :: AddrChan -> SockAddr -> IO ()
updateNeighbours _ _ = undefined
-- Read out the list of nodes
-- Choose the new socket's neighbours
-- Notify everyone of their new neighbours
