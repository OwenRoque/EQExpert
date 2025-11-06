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
> _**Update**: YA se habilita configuraciones VXLAN de Calico aquí._

## 3. Verificar Conectividad con Nodos
```bash
ansible all -i kubespray/inventory/eqexpert/inventory.ini -m ping -u ubuntu --private-key ~/.ssh/k8s_key.pem --vault-password-file ansible/vault.pass
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
