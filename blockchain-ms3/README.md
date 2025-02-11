# blockchain-ms3

## ✅ 1 Applying Kubernetes Manifests



```sh
kubectl apply -f postgres-config.yaml
kubectl apply -f postgres-secrets.yaml
kubectl apply -f postgres-pvc.yaml
kubectl apply -f postgres-deployment.yaml
kubectl apply -f postgres-service.yaml

Check out if was created correctly:

```sh
kubectl get pods
kubectl get svc
kubectl get pvc
kubectl get configmap
kubectl get secrets




