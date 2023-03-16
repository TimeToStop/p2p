import subprocess
import time
import re



SERVER_CMD = ["opam", "exec", "--", "dune", "exec", "server"]
CLIENT_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9000", "d", "127.0.0.1", "9002"]
CLIENT1_CMD = ["opam", "exec", "--", "dune", "exec", "client", "9001", "d", "127.0.0.1", "9002"]


def send_message(client, message):
    client.stdin.write('{}\n'.format(message).encode())
    client.stdin.flush()
    time.sleep(0.1)

def close_client(client):
    client.stdin.write('\\q\n'.encode())
    client.stdin.flush()
    time.sleep(0.1)
    client.kill()

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

        # Используйте регулярное выражение, чтобы найти строки, содержащие нужную подстроку
        match = re.search(rb"server: \[INFO\] request=msg 127\.0\.0\.1 (\d+) response=accept", line)
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

        # Используйте регулярное выражение, чтобы найти строки, содержащие нужную подстроку
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

        # Используйте регулярное выражение, чтобы найти строки, содержащие нужную подстроку
        match = re.search(rb"username:", line)
        if match:
            k += 1
    return k

def test_join_peer():
    # запускаем сервер и два экземпляра клиентского приложения
    print("Start server")
    servr_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert servr_proc.poll() is None

    print("Start client 1")
    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    print("Start client 2")
    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None


    close_client(client1_proc)
    close_client(client2_proc)

    servr_proc.kill()

    get_info(client1_proc)
    get_info(client2_proc)

    ports = find_ports(servr_proc)
    assert ports[0] == client1_proc.args[6]
    assert ports[1] == client2_proc.args[6]

    print("Test passed")


def test_chat():
    # запускаем сервер и два экземпляра клиентского приложения
    print("Start server")
    server_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert server_proc.poll() is None

    print("Start client 1")
    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    print("Start client 2")
    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None


    # Send messages between clients
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

test_chat()

def test_disconnect():
    # запускаем сервер и клиентский процесс
    print("Start server")
    servr_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert servr_proc.poll() is None

    print("Start client")
    client_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client_proc.poll() is None

    # отключаем клиента
    close_client(client_proc)

    # проверяем, что клиент отключен и больше не отображается в списке активных клиентов
    # assert "Active clients: 0" in servr_proc.stdout.readline().decode()

    servr_proc.kill()

    get_info(client_proc)
    get_info(servr_proc)


def test_history():
    # запускаем сервер и два экземпляра клиентского приложения
    print("Start server")
    server_proc = subprocess.Popen(SERVER_CMD, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert server_proc.poll() is None

    print("Start client 1")
    client1_proc = subprocess.Popen(CLIENT_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client1_proc.poll() is None

    # Send messages between clients
    send_message(client1_proc, "Hello, client2!")
    send_message(client1_proc, "Did you catch the game last night?")
    send_message(client1_proc, "It was crazy! We won in overtime. Bye!")

    print("Start client 2")
    client2_proc = subprocess.Popen(CLIENT1_CMD, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    time.sleep(3)
    assert client2_proc.poll() is None

    close_client(client1_proc)
    close_client(client2_proc)
    server_proc.kill()

    k1 = check_history(client2_proc)

    assert k1 == 3

test_history()

