aws_ecr_login() {
    aws ecr get-login-password | docker login --username AWS --password-stdin https://$(aws sts get-caller-identity --query 'Account' --output text).dkr.ecr.$(aws configure get region).amazonaws.com
}
