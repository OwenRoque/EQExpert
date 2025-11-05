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

## 3. Verificar Conectividad con Instancias
```bash
ansible all -i kubespray/inventory/mycluster/inventory.ini -m ping -u ubuntu --private-key ~/.ssh/k8s_key.pem --vault-password-file ansible/vault.pass
```

## 4. Desplegar Kubernetes (con Kubespray)
> _Es crucial el **cd** antes de ejecutar el playbook de Kubespray._
```bash
cd kubespray
ansible-playbook -i inventory/mycluster/inventory.ini --become --become-user=root cluster.yml -u ubuntu --private-key ~/.ssh/k8s_key.pem -e kube_version=1.33.5
```

## 5. Desplegar Aplicacion
```bash
cd ..
ansible-playbook -i kubespray/inventory/mycluster/inventory.ini ansible/playbooks/03_deploy_app.yaml
```
