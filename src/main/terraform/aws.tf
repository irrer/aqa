
provider "aws" {
  region = "us-west-2"
}


# terraform import aws_instance.AQA i-0922f98f35beab8e3
resource "aws_instance" "AQA" {
  instance_type = "t2.micro"
  ami           = "ami-1562d075"
  tags          {
    Name = "AQA"
  }
}


# terraform import aws_instance.AQATest i-0f401d3d7c82ee45a
resource "aws_instance" "AQATest" {
  instance_type = "t2.micro"
  ami           = "ami-cbce62b3"
  tags          {
    Name = "AQATest"
  }
}



resource "aws_ami" "aqa_backup" {
}



# terraform import aws_vpc.AQAVpc vpc-f440d393
resource "aws_vpc" "AQAVpc" {
  cidr_block = "172.31.0.0/16"
  tags { }
}


# terraform import aws_route53_zone.aqa_route Z2EXPG3EISFNO6
resource "aws_route53_zone" "aqa_route" {
  name = "automatedqualityassurance.org."
  comment = "Managed by Terraform"
  force_destroy = "false"
  tags { }
}

# terraform import aws_db_instance.aqa_db aqa
resource "aws_db_instance" "aqa_db" {
  instance_class = "db.t2.micro"
  publicly_accessible = "true"
  skip_final_snapshot = "true"
  tags          {
    workload-type = "other"
  }
}

# terraform import aws_db_instance.aqa_db_test aqatest
resource "aws_db_instance" "aqa_db_test" {
  instance_class = "db.t2.micro"
  publicly_accessible = "true"
  skip_final_snapshot = "true"
  tags          {
   workload-type  = "other"
  }
}

