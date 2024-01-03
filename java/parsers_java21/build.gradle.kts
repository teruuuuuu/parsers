plugins {
    java
    `java-library`
}

version = "0.0.1"

defaultTasks("clean", "build")

repositories {
    mavenCentral()
}

gradle.projectsEvaluated {
    tasks {
        withType<JavaCompile> {
            options.compilerArgs.add("-Xlint:unchecked")
            options.compilerArgs.add("-Xlint:deprecation")
        }
    }
}

dependencies {
    testImplementation(platform("org.junit:junit-bom:5.9.1"))
    testImplementation("org.junit.jupiter:junit-jupiter")
}

tasks.test {
    useJUnitPlatform()
}