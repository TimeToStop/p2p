import subprocess
import time
import re
import socket



SERVER_CMD = ["opam", "exec", "--", "dune", "exec", "server"]
CLIENT_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9010", "d", "127.0.0.1", "9002"]
CLIENT1_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9011", "d", "127.0.0.1", "9002"]


def send_message(client, message):
    client.stdin.write('{}\n'.format(message).encode())
    client.stdin.flush()
    time.sleep(0.1)

def close_client(client):
    client.stdin.write('\\q\n'.encode())
    client.stdin.flush()
    time.sleep(0.1)
    client.kill()
    time.sleep(0.1)

def get_info(client):
    while True:
        line = client.stderr.readline()
        if not line:
            break

        print(line)
        print("\n" + "---------------------------" + "\n")

def find_ports(server):
    ports = []
    while True:
        line = server.stderr.readline()
        if not line:
            break

        match = re.search(rb"server: \[INFO\] request=join 127\.0\.0\.1 (\d+) response=", line)
        if match:
            port = match.group(1).decode()
            ports.append(port)
    return ports

def check_msg(client):
    k = 0
    while True:
        line = client.stderr.readline()
        if not line:
            break

        match = re.search(rb"response=accepted", line)
        if match:
            k += 1
    return k

def check_history(client):
    k = 0
    while True:
        line = client.stderr.readline()
        if not line:
            break

        match = re.search(rb"username:", line)
        if match:
            k += 1
    return k

def test_join_peer():
    # запускаем сервер и два экземпляра клиентского приложения
    servr_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert servr_proc.poll() is None

    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None


    close_client(client1_proc)
    close_client(client2_proc)

    servr_proc.kill()

    # get_info(client1_proc)
    # get_info(client2_proc)

    ports = find_ports(servr_proc)
    assert ports[0] == client1_proc.args[6]
    assert ports[1] == client2_proc.args[6]

    print("Test join peer passed")


def test_chat():
    # запускаем сервер и два экземпляра клиентского приложения
    server_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert server_proc.poll() is None

    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None


    send_message(client1_proc, "Hello, client2!")
    send_message(client2_proc, "Hello, client1!")
    send_message(client1_proc, "Did you catch the game last night?")
    send_message(client2_proc, "Nah, I missed it. How was it?")
    send_message(client1_proc, "It was crazy! We won in overtime. Bye!")
    send_message(client2_proc, "Sick! I'll have to check out the highlights later. Bye")

    close_client(client1_proc)
    close_client(client2_proc)
    server_proc.kill()

    k1 = check_msg(client1_proc)
    k2 = check_msg(client2_proc)

    assert k1 == 3
    assert k2 == 3
    print("Test chat passed")


def test_disconnect():
    # запускаем сервер и клиентский процесс
    servr_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert servr_proc.poll() is None

    client_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client_proc.poll() is None

    close_client(client_proc)

    servr_proc.kill()

    get_info(client_proc)
    get_info(servr_proc)

    print("Test disconnect peer passed")


def test_history():
    # запускаем сервер и два экземпляра клиентского приложения
    server_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert server_proc.poll() is None

    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    send_message(client1_proc, "Hello, client2!")
    send_message(client1_proc, "Did you catch the game last night?")
    send_message(client1_proc, "It was crazy! We won in overtime. Bye!")

    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None

    close_client(client1_proc)
    close_client(client2_proc)
    server_proc.kill()

    k1 = check_history(client2_proc)

    assert k1 == 3

    print("Test history passed")
test_history()
test_chat()
test_join_peer()


def main():
    test_join_peer()
    time.sleep(1)
    test_chat()
    time.sleep(1)
    test_disconnect()
    time.sleep(1)
    test_history()

if __name__ == "__main__":
    main()