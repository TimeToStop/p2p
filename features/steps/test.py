from behave import given, when, then
from test_code import (
    start_server,
    start_client1,
    start_client2,
    start_client3,
    start_client4,
    close_client,
    send_message,
    check_msg,
    check_history,
    find_ports,
    get_port_client_close,
    check_connect_client
)

@given("a running server and two clients")
def step_given_server_two_clients(context):
    context.server_proc = start_server()
    context.client1_proc = start_client3()
    context.client2_proc = start_client4()

@when("the clients connect to the server")
def step_when_clients_connect(context):
    check_connect_client(context.client1_proc)
    check_connect_client(context.client2_proc)


@then("the server should recognize the connected clients")
def step_then_server_recognize_clients(context):
    close_client(context.client1_proc)
    close_client(context.client2_proc)
    context.server_proc.kill()

    ports = find_ports(context.server_proc)
    assert ports[0] == context.client1_proc.args[6]
    assert ports[1] == context.client2_proc.args[6]


@when("the clients exchange messages")
def step_when_clients_exchange_messages(context):
    send_message(context.client1_proc, "Hello, client2!")
    send_message(context.client2_proc, "Hello, client1!")
    send_message(context.client1_proc, "Did you catch the game last night?")
    send_message(context.client2_proc, "Nah, I missed it. How was it?")
    send_message(context.client1_proc, "It was crazy! We won in overtime. Bye!")
    send_message(context.client2_proc, "Sick! I'll have to check out the highlights later. Bye")

@then("each client should receive the correct number of messages")
def step_then_clients_receive_messages(context):
    close_client(context.client1_proc)
    close_client(context.client2_proc)
    context.server_proc.kill()
    k1 = check_msg(context.client1_proc)
    k2 = check_msg(context.client2_proc)
    assert k1 == 3
    assert k2 == 3

@given("a running server and a connected client")
def step_given_server_connected_client(context):
    context.server_proc = start_server()
    context.client_proc = start_client1()
#
@when("the client disconnects")
def step_when_client_disconnects(context):
    close_client(context.client_proc)

@then("the server should recognize the disconnected client")
def step_then_server_recognize_disconnected_client(context):
    context.server_proc.kill()
    port = get_port_client_close(context.server_proc)
    assert port[0] == context.client_proc.args[6]

@given("a running server and a connected client sending messages")
def step_given_server_connected_client_sending_messages(context):
    context.server_proc = start_server()
    context.client1_proc = start_client1()
    send_message(context.client1_proc, "Hello, client2!")
    send_message(context.client1_proc, "Did you catch the game last night?")
    send_message(context.client1_proc, "It was crazy! We won in overtime. Bye!")

@when("another client connects")
def step_when_another_client_connects(context):
    context.client2_proc = start_client2()

@then("the new client should receive the message history")
def step_then_new_client_receive_message_history(context):
    close_client(context.client1_proc)
    close_client(context.client2_proc)
    context.server_proc.kill()
    k1 = check_history(context.client2_proc)
    assert k1 == 3
