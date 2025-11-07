# Ejecución de secuencia completa de despliegue del proyecto
[TODO] Agregar pasos/prerequisitos previos del documento aquí
## 1. Activar Virtual Environment
```bash
source ansible-venv/bin/activate
```

## 2. Provisionar Infraestructura
```bash
ansible-playbook infrastructure_deploy.yaml
```
**Custom Cluster Setup Configurations:**
1. [x] _calico_network_backend_
2. [x] _metrics_server_

## 3. Verificar Conectividad con Nodos
```bash
ansible all -i kubespray/inventory/eqexpert/inventory.ini -m ping
```

## 4. Desplegar Kubernetes (con Kubespray)
> _Es crucial el **cd** antes de ejecutar el playbook de Kubespray:_
```bash
cd kubespray
ansible-playbook -i inventory/eqexpert/inventory.ini --become --become-user=root cluster.yml -u ubuntu --private-key ~/.ssh/k8s_key.pem -e kube_version=1.33.5
```

## 5. Desplegar EQExpert
```bash
cd ..
ansible-playbook -i kubespray/inventory/eqexpert/inventory.ini ansible/playbooks/04_deploy_app.yaml
```

## 6. Desplegar Self-Hosted Runner (CI/CD pipelines)
```bash
ansible-playbook -i kubespray/inventory/eqexpert/inventory.ini ansible/playbooks/05_install_runner.yaml --vault-password-file ansible/vault.pass --timeout=30
```