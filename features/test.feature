Feature: Peer-to-peer chat application

  Scenario: Joining a peer
    Given a running server and two clients
    When the clients connect to the server
    Then the server should recognize the connected clients

  Scenario: Chatting between clients
    Given a running server and two clients
    When the clients exchange messages
    Then each client should receive the correct number of messages

  Scenario: Disconnecting a client
    Given a running server and a connected client
    When the client disconnects
    Then the server should recognize the disconnected client

  Scenario: Message history
    Given a running server and a connected client sending messages
    When another client connects
    Then the new client should receive the message history